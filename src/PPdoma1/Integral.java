package PPdoma1;

import java.util.concurrent.atomic.DoubleAdder;
// создаем класс Интеграл
public class Integral {
    public static final int STEPS = 10_000_000;
    public static final int THREADS = 6;

    static class Acc {
        private double acc = 0.0;

        public synchronized void addToAcc(double value) {
            acc += value;
        }

        public double getAcc() {
            return acc;
        }
    }

    public static double function(double x, int functionNumber) {
        if (functionNumber == 1) {
            return x * x * x;
        }

        return Math.sin(x) * Math.cos(x)
                + Math.sqrt(x + 1.0)
                + Math.log(x + 1.0);
    }

    public static Thread taskThread(
            int n,
            double a,
            double dx,
            double[] results,
            int functionNumber
    ) {
        return new Thread(() -> {
            int start = n * STEPS / THREADS;
            int finish = (n + 1) * STEPS / THREADS;
            double acc = 0.0;

            for (int i = start; i < finish; i++) {
                double x = a + (i + 0.5) * dx;
                acc += function(x, functionNumber) * dx;
            }

            results[n] = acc;
        });
    }

    public static Thread taskMonitorThread(
            int n,
            double a,
            double dx,
            Acc acc,
            int functionNumber
    ) {
        return new Thread(() -> {
            int start = n * STEPS / THREADS;
            int finish = (n + 1) * STEPS / THREADS;

            for (int i = start; i < finish; i++) {
                double x = a + (i + 0.5) * dx;
                double value = function(x, functionNumber) * dx;
                acc.addToAcc(value);
            }
        });
    }

    public static Thread taskAtomicThread(
            int n,
            double a,
            double dx,
            DoubleAdder acc,
            int functionNumber
    ) {
        return new Thread(() -> {
            int start = n * STEPS / THREADS;
            int finish = (n + 1) * STEPS / THREADS;

            for (int i = start; i < finish; i++) {
                double x = a + (i + 0.5) * dx;
                double value = function(x, functionNumber) * dx;
                acc.add(value);
            }
        });
    }

    public static void measureSequential(
            double a,
            double b,
            int functionNumber
    ) {
        double dx = (b - a) / STEPS;
        long start = System.nanoTime();
        double acc = 0.0;

        for (int i = 0; i < STEPS; i++) {
            double x = a + (i + 0.5) * dx;
            acc += function(x, functionNumber) * dx;
        }

        long finish = System.nanoTime();

        System.out.println("Sequential result: " + acc);
        System.out.println("Sequential time (ms): "
                + (double) (finish - start) / 1_000_000);
    }

    public static void measureParallel(
            double a,
            double b,
            int functionNumber
    ) throws InterruptedException {
        double dx = (b - a) / STEPS;
        Thread[] threads = new Thread[THREADS];
        double[] results = new double[THREADS];
        long start = System.nanoTime();

        for (int i = 0; i < THREADS; i++) {
            threads[i] = taskThread(i, a, dx, results, functionNumber);
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].start();
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].join();
        }

        double acc = 0.0;
        for (int i = 0; i < THREADS; i++) {
            acc += results[i];
        }

        long finish = System.nanoTime();

        System.out.println("Parallel local result: " + acc);
        System.out.println("Parallel local time (ms): "
                + (double) (finish - start) / 1_000_000);
    }

    public static void measureMonitor(
            double a,
            double b,
            int functionNumber
    ) throws InterruptedException {
        double dx = (b - a) / STEPS;
        Thread[] threads = new Thread[THREADS];
        Acc acc = new Acc();
        long start = System.nanoTime();

        for (int i = 0; i < THREADS; i++) {
            threads[i] = taskMonitorThread(i, a, dx, acc, functionNumber);
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].start();
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].join();
        }

        long finish = System.nanoTime();

        System.out.println("Monitor result: " + acc.getAcc());
        System.out.println("Monitor time (ms): "
                + (double) (finish - start) / 1_000_000);
    }

    public static void measureAtomic(
            double a,
            double b,
            int functionNumber
    ) throws InterruptedException {
        double dx = (b - a) / STEPS;
        Thread[] threads = new Thread[THREADS];
        DoubleAdder acc = new DoubleAdder();
        long start = System.nanoTime();

        for (int i = 0; i < THREADS; i++) {
            threads[i] = taskAtomicThread(i, a, dx, acc, functionNumber);
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].start();
        }

        for (int i = 0; i < THREADS; i++) {
            threads[i].join();
        }

        long finish = System.nanoTime();

        System.out.println("Atomic result: " + acc.sum());
        System.out.println("Atomic time (ms): "
                + (double) (finish - start) / 1_000_000);
    }

    public static void runTest(
            String name,
            double a,
            double b,
            int functionNumber
    ) throws InterruptedException {
        System.out.println();
        System.out.println("==============================");
        System.out.println(name);
        System.out.println("==============================");

        measureSequential(a, b, functionNumber);
        measureParallel(a, b, functionNumber);
        measureAtomic(a, b, functionNumber);
        measureMonitor(a, b, functionNumber);
    }

    public static void main(String[] args) throws InterruptedException {
        runTest("PPdoma1.Integral 1: x^3 on [0, 2]", 0.0, 2.0, 1);
        runTest(
                "PPdoma1.Integral 2: sin(x) * cos(x) + sqrt(x + 1) + ln(x + 1) on [0, 10]",
                0.0,
                10.0,
                2
        );
    }
}
