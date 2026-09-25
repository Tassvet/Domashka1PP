import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SplittableRandom;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicIntegerArray;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/*
 * Практика 1. Lock, инварианты и deadlock.
 *
 * Компиляция:
 *   javac Practice01LocksExercises.java
 *
 * Запуск задач:
 *   java P1StatisticsExercise
 *   java P1TransferExercise
 *   java P1WarehouseExercise
 *   java P1DiningPhilosophersHomework
 *
 * Все классы намеренно находятся в одном файле. У каждой задачи свой main.
 */
public class Practice01LocksExercises {
    public static void main(String[] args) {
        System.out.println("Compile this file, then run one of these classes:");
        System.out.println("  P1StatisticsExercise");
        System.out.println("  P1TransferExercise");
        System.out.println("  P1WarehouseExercise");
        System.out.println("  P1DiningPhilosophersHomework");
    }
}


class P1StatisticsExercise {
    /*
     * ЗАДАЧА 1. Потокобезопасная статистика
     *
     * Несколько потоков одновременно передают числа в один объект Statistics.
     * Реализуйте add и snapshot с помощью одного Lock.
     *
     * Поля count, sum, min и max описывают один набор чисел и образуют единый
     * инвариант. Snapshot должен содержать согласованные значения всех полей.
     * Для пустой статистики min должен быть Integer.MAX_VALUE, а max —
     * Integer.MIN_VALUE.
     */
    private record Snapshot(long count, long sum, int min, int max) {
    }

    private static final class Statistics {
        private final Lock lock = new ReentrantLock();
        private long count;
        private long sum;
        private int min = Integer.MAX_VALUE;
        private int max = Integer.MIN_VALUE;

        void add(int value) {
            // TODO: атомарно обновите все четыре поля.
            throw new UnsupportedOperationException("add is not implemented");
        }

        Snapshot snapshot() {
            // TODO: верните согласованный снимок всех полей.
            throw new UnsupportedOperationException("snapshot is not implemented");
        }
    }

    public static void main(String[] args) throws Exception {
        int[] values = new int[200_000];
        SplittableRandom random = new SplittableRandom(42);

        long expectedSum = 0;
        int expectedMin = Integer.MAX_VALUE;
        int expectedMax = Integer.MIN_VALUE;
        for (int i = 0; i < values.length; i++) {
            values[i] = random.nextInt(-1_000_000, 1_000_001);
            expectedSum += values[i];
            expectedMin = Math.min(expectedMin, values[i]);
            expectedMax = Math.max(expectedMax, values[i]);
        }

        Statistics statistics = new Statistics();
        int workers = 8;
        ExecutorService pool = Executors.newFixedThreadPool(workers);
        List<Future<?>> results = new ArrayList<>();

        try {
            for (int worker = 0; worker < workers; worker++) {
                int from = worker * values.length / workers;
                int to = (worker + 1) * values.length / workers;
                results.add(pool.submit(() -> {
                    for (int i = from; i < to; i++) {
                        statistics.add(values[i]);
                    }
                }));
            }

            for (Future<?> result : results) {
                result.get(10, TimeUnit.SECONDS);
            }
        } catch (TimeoutException e) {
            throw new AssertionError("The statistics task did not finish in time", e);
        } finally {
            pool.shutdownNow();
        }

        Snapshot actual = statistics.snapshot();
        Snapshot expected = new Snapshot(
                values.length, expectedSum, expectedMin, expectedMax);

        if (!actual.equals(expected)) {
            throw new AssertionError("Expected " + expected + ", got " + actual);
        }

        System.out.println("OK: count, sum, min and max are correct");
    }

    /*
     * Вывод, означающий, что задача, скорее всего, решена правильно:
     * OK: count, sum, min and max are correct
     */
}


class P1TransferExercise {
    /*
     * ЗАДАЧА 2. Переводы между счетами
     *
     * Реализуйте transfer(from, to, amount).
     *
     * Требования:
     *   1) перевод выполняется целиком либо не выполняется;
     *   2) баланс from не может стать отрицательным;
     *   3) у каждого Account свой Lock; общий глобальный Lock запрещён;
     *   4) встречные переводы не должны приводить к deadlock;
     *   5) Locks необходимо всегда захватывать в едином порядке по account.id;
     *   6) при недостатке денег вернуть false, иначе выполнить перевод и
     *      вернуть true.
     *
     * amount всегда положителен, from и to — разные счета.
     */
    private static final class Account {
        private final int id;
        private final Lock lock = new ReentrantLock();
        private long balance;

        private Account(int id, long initialBalance) {
            this.id = id;
            this.balance = initialBalance;
        }

        long balance() {
            lock.lock();
            try {
                return balance;
            } finally {
                lock.unlock();
            }
        }
    }

    private static boolean transfer(Account from, Account to, long amount) {
        // TODO: захватите оба Lock в порядке id и выполните перевод.
        throw new UnsupportedOperationException("transfer is not implemented");
    }

    public static void main(String[] args) throws Exception {
        int accountCount = 100;
        long initialBalance = 10_000;
        int workers = 8;
        int attemptsPerWorker = 20_000;
        long expectedTotal = accountCount * initialBalance;

        List<Account> accounts = new ArrayList<>();
        for (int id = 0; id < accountCount; id++) {
            accounts.add(new Account(id, initialBalance));
        }

        ExecutorService pool = Executors.newFixedThreadPool(workers);
        List<Future<?>> results = new ArrayList<>();

        try {
            for (int worker = 0; worker < workers; worker++) {
                int workerId = worker;
                results.add(pool.submit(() -> {
                    SplittableRandom random = new SplittableRandom(1_000 + workerId);
                    for (int i = 0; i < attemptsPerWorker; i++) {
                        int fromIndex = random.nextInt(accountCount);
                        int toIndex = random.nextInt(accountCount - 1);
                        if (toIndex >= fromIndex) {
                            toIndex++;
                        }
                        long amount = random.nextLong(1, 101);
                        transfer(accounts.get(fromIndex), accounts.get(toIndex), amount);
                    }
                }));
            }

            for (Future<?> result : results) {
                result.get(15, TimeUnit.SECONDS);
            }
        } catch (TimeoutException e) {
            throw new AssertionError("Possible deadlock: transfers did not finish", e);
        } finally {
            pool.shutdownNow();
        }

        long actualTotal = 0;
        for (Account account : accounts) {
            long balance = account.balance();
            if (balance < 0) {
                throw new AssertionError(
                        "Account " + account.id + " has negative balance " + balance);
            }
            actualTotal += balance;
        }

        if (actualTotal != expectedTotal) {
            throw new AssertionError(
                    "Expected total " + expectedTotal + ", got " + actualTotal);
        }

        int attempts = workers * attemptsPerWorker;
        System.out.println("OK: " + attempts
                + " transfer attempts completed, total balance=" + actualTotal);
    }

    /*
     * Вывод, означающий, что задача, скорее всего, решена правильно:
     * OK: 160000 transfer attempts completed, total balance=1000000
     */
}


class P1WarehouseExercise {
    /*
     * ЗАДАЧА 3. Перемещение товаров между складами
     *
     * Каждый товар изначально находится ровно на одном складе. Реализуйте
     * move(from, to, item), который атомарно переносит товар между складами.
     *
     * Метод возвращает false и ничего не меняет, если товара нет на from.
     * Иначе товар удаляется с from, добавляется на to, и метод возвращает true.
     *
     * Требования:
     *   1) у каждого Warehouse свой Lock; глобальный Lock запрещён;
     *   2) Locks необходимо захватывать в едином порядке по warehouse.id;
     *   3) встречные перемещения не должны приводить к deadlock;
     *   4) товар не должен исчезнуть или оказаться на двух складах.
     *
     * from и to всегда разные склады.
     */
    private static final class Warehouse {
        private final int id;
        private final Lock lock = new ReentrantLock();
        private final Set<String> items = new HashSet<>();

        private Warehouse(int id) {
            this.id = id;
        }

        void addInitialItem(String item) {
            items.add(item);
        }

        Set<String> snapshotItems() {
            lock.lock();
            try {
                return Set.copyOf(items);
            } finally {
                lock.unlock();
            }
        }
    }

    private static boolean move(
            Warehouse from,
            Warehouse to,
            String item
    ) {
        // TODO: захватите оба Lock в порядке id и атомарно перенесите товар.
        throw new UnsupportedOperationException("move is not implemented");
    }

    public static void main(String[] args) throws Exception {
        int warehouseCount = 16;
        int itemCount = 10_000;
        int workers = 8;
        int attemptsPerWorker = 20_000;

        List<Warehouse> warehouses = new ArrayList<>();
        for (int id = 0; id < warehouseCount; id++) {
            warehouses.add(new Warehouse(id));
        }
        for (int item = 0; item < itemCount; item++) {
            warehouses.get(item % warehouseCount).addInitialItem("item-" + item);
        }

        ExecutorService pool = Executors.newFixedThreadPool(workers);
        List<Future<?>> results = new ArrayList<>();

        try {
            for (int worker = 0; worker < workers; worker++) {
                int workerId = worker;
                results.add(pool.submit(() -> {
                    SplittableRandom random = new SplittableRandom(2_000 + workerId);
                    for (int i = 0; i < attemptsPerWorker; i++) {
                        int fromIndex = random.nextInt(warehouseCount);
                        int toIndex = random.nextInt(warehouseCount - 1);
                        if (toIndex >= fromIndex) {
                            toIndex++;
                        }
                        String item = "item-" + random.nextInt(itemCount);
                        move(warehouses.get(fromIndex), warehouses.get(toIndex), item);
                    }
                }));
            }

            for (Future<?> result : results) {
                result.get(15, TimeUnit.SECONDS);
            }
        } catch (TimeoutException e) {
            throw new AssertionError("Possible deadlock: moves did not finish", e);
        } finally {
            pool.shutdownNow();
        }

        Set<String> allItems = new HashSet<>();
        for (Warehouse warehouse : warehouses) {
            for (String item : warehouse.snapshotItems()) {
                if (!allItems.add(item)) {
                    throw new AssertionError("Duplicate item: " + item);
                }
            }
        }

        if (allItems.size() != itemCount) {
            throw new AssertionError(
                    "Expected " + itemCount + " items, got " + allItems.size());
        }
        for (int item = 0; item < itemCount; item++) {
            if (!allItems.contains("item-" + item)) {
                throw new AssertionError("Lost item: item-" + item);
            }
        }

        System.out.println("OK: " + itemCount
                + " items accounted for, duplicates=0");
    }

    /*
     * Вывод, означающий, что задача, скорее всего, решена правильно:
     * OK: 10000 items accounted for, duplicates=0
     */
}


class P1DiningPhilosophersHomework {
    /*
     * ДОМАШНЕЕ ЗАДАНИЕ. Обедающие философы. типа 5 философов 5 вилок и философ берет 2 вилки чтобы поесть. короче можно пронумеровать вилки от 1 до 5 и использовать от большей цифры к меньшей типа брать 5 и 4 и тд. то есть в строго определеннном порядке
     *
     * Философ с номером i использует вилки i и (i + 1) % philosopherCount.
     * Реализуйте Table.eat(philosopherId).
     *
     * Требования:
     *   1) перед eatWithBothForks должны быть захвачены обе нужные вилки;
     *   2) вилки всегда захватываются в порядке возрастания fork.id;
     *   3) вилки освобождаются в finally в обратном порядке;
     *   4) соседние философы не могут есть одновременно;
     *   5) программа должна завершиться без deadlock;
     *   6) tryLock с таймаутом использовать не нужно.
     *
     * Метод eatWithBothForks уже реализован. Он изображает приём пищи и
     * проверяет, что одну вилку не используют одновременно два философа.
     */
    private static final class Fork {
        private final int id;
        private final Lock lock = new ReentrantLock(true);

        private Fork(int id) {
            this.id = id;
        }
    }

    private static final class Table {
        private final Fork[] forks;
        private final AtomicIntegerArray forkUsers;
        private final AtomicBoolean conflictDetected = new AtomicBoolean();

        private Table(int philosopherCount) {
            forks = new Fork[philosopherCount];
            for (int id = 0; id < philosopherCount; id++) {
                forks[id] = new Fork(id);
            }
            forkUsers = new AtomicIntegerArray(philosopherCount);
        }

        void eat(int philosopherId) {
            Fork left = forks[philosopherId];
            Fork right = forks[(philosopherId + 1) % forks.length];

            // Всегда захватываем вилки в порядке возрастания id
            Fork first = left.id < right.id ? left : right;
            Fork second = left.id < right.id ? right : left;

            first.lock.lock();
            try {
                second.lock.lock();
                try {
                    eatWithBothForks(left, right);
                } finally {
                    second.lock.unlock();
                }
            } finally {
                first.lock.unlock();
            }
        }

        private void eatWithBothForks(Fork left, Fork right) {
            int leftUsers = forkUsers.incrementAndGet(left.id);
            int rightUsers = forkUsers.incrementAndGet(right.id);
            if (leftUsers != 1 || rightUsers != 1) {
                conflictDetected.set(true);
            }

            try {
                Thread.sleep(1);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            } finally {
                forkUsers.decrementAndGet(right.id);
                forkUsers.decrementAndGet(left.id);
            }
        }
    }

    public static void main(String[] args) throws Exception {
        int philosopherCount = 5;
        int mealsPerPhilosopher = 100;
        Table table = new Table(philosopherCount);
        ExecutorService pool = Executors.newFixedThreadPool(philosopherCount);
        List<Future<Integer>> results = new ArrayList<>();

        try {
            for (int philosopher = 0; philosopher < philosopherCount; philosopher++) {
                int philosopherId = philosopher;
                results.add(pool.submit(() -> {
                    int meals = 0;
                    while (meals < mealsPerPhilosopher) {
                        table.eat(philosopherId);
                        meals++;
                        Thread.yield();
                    }
                    return meals;
                }));
            }

            for (int philosopher = 0; philosopher < philosopherCount; philosopher++) {
                int meals = results.get(philosopher).get(10, TimeUnit.SECONDS);
                if (meals != mealsPerPhilosopher) {
                    throw new AssertionError(
                            "Philosopher " + philosopher + " ate " + meals + " times");
                }
            }
        } catch (TimeoutException e) {
            throw new AssertionError("Possible deadlock: philosophers did not finish", e);
        } finally {
            pool.shutdownNow();
        }

        if (table.conflictDetected.get()) {
            throw new AssertionError("Two philosophers used the same fork");
        }

        System.out.println("OK: every philosopher ate " + mealsPerPhilosopher
                + " times, no deadlock detected");
    }

    /*

     * OK: every philosopher ate 100 times, no deadlock detected
     */
}
/* Ответ на ваш вопрос к дз2:

 * Если для всех ресурсов задан полный порядок и каждый поток захватывает
 * ресурсы только в соответствии с этим порядком, то deadlock возникнуть
 * не может. Для deadlock необходимо циклическое ожидание ресурсов,
 * а единый порядок захвата не позволяет построить такой цикл
 *
 * Например, если A < B, поток может сначала взять A, а затем B.
 * Ситуация, когда один поток держит A и ждёт B, а другой держит B
 * и ждёт A, невозможна, так как второй поток нарушил бы общий порядок.
 *
 * Освобождение ресурсов в обратном порядке удобно для вложенных
 * блокировок, но для предотвращения deadlock принципиален именно
 * порядок захвата ресурсов.
 * Поэтому если освобождать ресурсы в том же порядке, в котором они
 * были взяты, deadlock также не возникнет, пока все захваты ресурсов
 * выполняются в соответствии с установленным полным порядком.
 */