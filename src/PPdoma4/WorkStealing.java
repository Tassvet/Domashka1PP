import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.Vector;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class WorkStealing {

    enum TaskDistribution {
        UNIFORM,
        PERIODIC,
        PARETO
    }

    interface Shutdownable {
        void shutdown();
    }

    public interface ShutdownableExecutor extends Shutdownable, Executor {
    }

    // Моя обертка над обычным FixedThreadPool.
    // Внутри находится готовый пул потоков из Java.
    static class MyFixedThreadPool implements ShutdownableExecutor {

        private final ExecutorService executor;

        MyFixedThreadPool(int threadNumber) {
            executor = Executors.newFixedThreadPool(threadNumber);
        }

        @Override
        public void execute(Runnable command) {
            executor.execute(command);
        }

        @Override
        public void shutdown() {
            // Сначала запрещаем добавлять новые задачи.
            executor.shutdown();

            try {
                // Потом ждем, пока закончатся уже добавленные задачи.
                executor.awaitTermination(1, TimeUnit.DAYS);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException(e);
            }
        }
    }

    static class ThreadPerTaskExecutor implements ShutdownableExecutor {
        List<Thread> threads = new ArrayList<>();

        @Override
        public void execute(Runnable command) {
            var thread = new Thread(command);
            threads.add(thread);
            thread.start();
        }

        @Override
        public void shutdown() {
            threads.forEach(thread -> {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException(e);
                }
            });
        }
    }

    static class RoundRobinExecutor implements ShutdownableExecutor {

        protected static final Runnable EXIT_TASK = () -> {
        };

        AtomicInteger counter = new AtomicInteger(0);
        List<Thread> threads;
        Vector<BlockingDeque<Runnable>> tasks;
        volatile boolean isShuttingDown = false;

        RoundRobinExecutor(int threadNumber) {
            Supplier<IntStream> stream =
                    () -> IntStream.iterate(0, x -> x < threadNumber, x -> x + 1);

            tasks = new Vector<>(
                    stream.get()
                            .mapToObj(x -> new LinkedBlockingDeque<Runnable>())
                            .toList()
            );

            threads = stream.get()
                    .mapToObj(this::spawnThread)
                    .toList();

            threads.forEach(Thread::start);
        }

        Thread spawnThread(int id) {
            return new Thread(() -> {
                while (true) {
                    Runnable task;

                    try {
                        task = tasks.get(id).takeFirst();
                    } catch (InterruptedException e) {
                        Thread.currentThread().interrupt();
                        return;
                    }

                    if (task == EXIT_TASK) {
                        return;
                    }

                    task.run();
                }
            });
        }

        @Override
        public void shutdown() {
            synchronized (this) {
                if (!isShuttingDown) {
                    isShuttingDown = true;
                    tasks.forEach(queue -> queue.addLast(EXIT_TASK));
                }
            }

            threads.forEach(thread -> {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    throw new RuntimeException(e);
                }
            });
        }

        @Override
        public synchronized void execute(Runnable command) {
            if (isShuttingDown) {
                throw new RejectedExecutionException("Executor is shutting down");
            }

            int queueNumber = counter.addAndGet(1) % threads.size();
            tasks.get(queueNumber).addLast(command);
        }
    }

    static class WorkStealingExecutor extends RoundRobinExecutor {

        WorkStealingExecutor(int threadNumber) {
            super(threadNumber);
        }

        @Override
        Thread spawnThread(int id) {
            return new Thread(() -> {
                while (true) {
                    Runnable task = tasks.get(id).pollFirst();

                    if (task == null) {
                        task = stealTask(id);
                    }

                    if (task == null) {
                        try {
                            task = tasks.get(id).pollFirst(1, TimeUnit.MILLISECONDS);
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                            return;
                        }

                        if (task == null) {
                            continue;
                        }
                    }

                    if (task == EXIT_TASK) {
                        task = stealTask(id);

                        if (task == null) {
                            return;
                        }

                        tasks.get(id).addLast(EXIT_TASK);
                    }

                    task.run();
                }
            });
        }

        private Runnable stealTask(int thiefId) {
            for (int offset = 1; offset < tasks.size(); offset++) {
                int queueNumber = (thiefId + offset) % tasks.size();
                var queue = tasks.get(queueNumber);
                var task = queue.pollLast();

                if (task == EXIT_TASK) {
                    task = queue.pollLast();
                    queue.addLast(EXIT_TASK);
                }

                if (task != null) {
                    return task;
                }
            }

            return null;
        }
    }

    static final int THREAD_NUMBER = 10;

    // В исходном файле было 100 000 задач.
    // Для ThreadPerTaskExecutor это означает создание 100 000 потоков.
    // Поэтому для безопасного запуска на обычном компьютере я взял 2 000 задач.
    static final int TASK_NUMBER = 2_000;

    static final int TARGET_OPTIMAL_FULL_TIME = 1_000;

    static final int MEAN_TASK_TIME = (int) Math.round(
            (TARGET_OPTIMAL_FULL_TIME + 0.0) / TASK_NUMBER * THREAD_NUMBER
    );

    static final int LOWER_TASK_TIME_BOUND = 0;
    static final int HIGHER_TASK_TIME_BOUND = MEAN_TASK_TIME * 2 + 1;
    static final int ESTIMATED_OPTIMAL_TIME =
            MEAN_TASK_TIME * TASK_NUMBER / THREAD_NUMBER;
    static final long TARGET_TOTAL_TASK_TIME =
            (long) TARGET_OPTIMAL_FULL_TIME * THREAD_NUMBER;
    static final long RANDOM_SEED = 42;
    static final double PARETO_SHAPE = 1.5;
    static final int PARETO_RAW_MAX_TASK_TIME = MEAN_TASK_TIME * 100;

    // Один миллисекундный шаг превращаем в такое количество повторений цикла.
    // Это нужно только для того, чтобы blackHole работал достаточно заметное время.
    static final long BLACK_HOLE_MULTIPLIER = 100_000;

    // volatile не дает Java просто выбросить бесполезный результат вычислений.
    static volatile long blackHoleResult;

    // Функция делает ровно difficulty повторений цикла.
    // Поэтому сложность функции линейная: O(difficulty).
    static void blackHole(long difficulty) {
        long result = 1;

        for (long i = 0; i < difficulty; i++) {
            result = result * 31 + i;
        }

        blackHoleResult = result;
    }

    static Runnable createTask(int duration, boolean useBlackHole) {
        if (useBlackHole) {
            return () -> blackHole(duration * BLACK_HOLE_MULTIPLIER);
        }

        return () -> {
            try {
                Thread.sleep(duration);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                throw new RuntimeException(e);
            }
        };
    }

    static int createTaskDuration(
            TaskDistribution distribution,
            int taskId,
            Random random
    ) {
        return switch (distribution) {
            case UNIFORM -> random.nextInt(LOWER_TASK_TIME_BOUND, HIGHER_TASK_TIME_BOUND);

            case PERIODIC -> taskId % THREAD_NUMBER == 0
                    ? MEAN_TASK_TIME * THREAD_NUMBER
                    : 0;

            case PARETO -> {
                double scale = MEAN_TASK_TIME * (PARETO_SHAPE - 1) / PARETO_SHAPE;
                double duration = scale /
                        Math.pow(1 - random.nextDouble(), 1 / PARETO_SHAPE);

                yield (int) Math.min(
                        Math.round(duration),
                        PARETO_RAW_MAX_TASK_TIME
                );
            }
        };
    }

    static int[] createTaskDurations(TaskDistribution distribution) {
        Random random = new Random(RANDOM_SEED);
        int[] durations = new int[TASK_NUMBER];

        long totalDuration = 0;

        for (int taskId = 0; taskId < TASK_NUMBER; taskId++) {
            durations[taskId] = createTaskDuration(distribution, taskId, random);
            totalDuration += durations[taskId];
        }

        double scale = (double) TARGET_TOTAL_TASK_TIME / totalDuration;
        double remainder = 0;
        long normalizedTotal = 0;

        for (int taskId = 0; taskId < TASK_NUMBER; taskId++) {
            double scaledDuration = durations[taskId] * scale + remainder;
            durations[taskId] = (int) scaledDuration;
            remainder = scaledDuration - durations[taskId];
            normalizedTotal += durations[taskId];
        }

        for (int taskId = 0;
             normalizedTotal < TARGET_TOTAL_TASK_TIME;
             taskId++) {

            durations[taskId % TASK_NUMBER]++;
            normalizedTotal++;
        }

        for (int taskId = 0;
             normalizedTotal > TARGET_TOTAL_TASK_TIME;
             taskId++) {

            int index = taskId % TASK_NUMBER;

            if (durations[index] > 0) {
                durations[index]--;
                normalizedTotal--;
            }
        }

        return durations;
    }

    static Vector<Runnable> createTasks(
            TaskDistribution distribution,
            boolean useBlackHole
    ) {
        int[] durations = createTaskDurations(distribution);
        Vector<Runnable> tasks = new Vector<>();

        for (int i = 0; i < TASK_NUMBER; i++) {
            tasks.add(createTask(durations[i], useBlackHole));
        }

        return tasks;
    }

    public record Pair<A, B>(A first, B second) {
    }

    public static Pair<Long, Long> measureExecutor(
            ShutdownableExecutor executor,
            TaskDistribution distribution,
            boolean useBlackHole
    ) {
        Vector<Runnable> tasks = createTasks(distribution, useBlackHole);

        long start = System.nanoTime();

        tasks.forEach(executor::execute);

        long submissionEnd = System.nanoTime();

        executor.shutdown();

        long finish = System.nanoTime();

        return new Pair<>(submissionEnd - start, finish - start);
    }

    static void printResult(String name, Pair<Long, Long> result) {
        double submissionMilliseconds = result.first() / 1_000_000.0;
        double fullMilliseconds = result.second() / 1_000_000.0;

        System.out.printf(
                "%-24s добавление: %8.2f мс, все время: %8.2f мс%n",
                name,
                submissionMilliseconds,
                fullMilliseconds
        );
    }

    static void measureAll(boolean useBlackHole) {
        if (useBlackHole) {
            System.out.println("\n===== BLACK HOLE =====");
        } else {
            System.out.println("\n===== SLEEP =====");
        }

        for (TaskDistribution distribution : TaskDistribution.values()) {
            System.out.println("\nРаспределение задач: " + distribution);

            printResult(
                    "My FixedThreadPool",
                    measureExecutor(
                            new MyFixedThreadPool(THREAD_NUMBER),
                            distribution,
                            useBlackHole
                    )
            );

            printResult(
                    "Thread per task",
                    measureExecutor(
                            new ThreadPerTaskExecutor(),
                            distribution,
                            useBlackHole
                    )
            );

            printResult(
                    "Round Robin",
                    measureExecutor(
                            new RoundRobinExecutor(THREAD_NUMBER),
                            distribution,
                            useBlackHole
                    )
            );

            printResult(
                    "Work Stealing",
                    measureExecutor(
                            new WorkStealingExecutor(THREAD_NUMBER),
                            distribution,
                            useBlackHole
                    )
            );
        }
    }

    public static void main(String[] args) {
        System.out.println("Количество потоков: " + THREAD_NUMBER);
        System.out.println("Количество задач: " + TASK_NUMBER);
        System.out.println("Ожидаемое оптимальное время: " + ESTIMATED_OPTIMAL_TIME + " мс");

        measureAll(false);
        measureAll(true);
    }
    /*Ниже записаны результаты моего одного запуска программы, на другом пк числа могут(и будут) отличаться
     *
     * SLEEP, время выполнения всех задач в миллисекундах:
     *
     *                         UNIFORM    PERIODIC    PARETO
     * My FixedThreadPool      1105,48     1122,70   1220,28
     * Thread per task          147,46      250,21    410,70
     * Round Robin             1210,91    11212,69   1324,14
     * Work Stealing           1109,08     1131,30   1215,31
     *
     * BLACK HOLE, время выполнения всех задач в миллисекундах:
     *
     *                         UNIFORM    PERIODIC    PARETO
     * My FixedThreadPool       336,93      299,99    311,97
     * Thread per task          463,45      420,39    449,84
     * Round Robin              311,28     1067,09    324,40
     * Work Stealing            318,73      326,91    320,90
     */
}