package PPdoma3;

public class Mandelbrot {

    //Задаем размер условного экрана
    static int width = 800;
    static int height = 600;

    // Максимальное количество повторений вычисления
    static int maxIterations = 1000;

    // Результат для каждого пикселя (true - пиксель входит в множество, false - пиксель не входит в множество)

    static boolean[][] result = new boolean[height][width];

    public static void main(String[] args) throws InterruptedException {

        // Число потоков, можем менять его для сравнения по времени
        int threadCount = 4;

        Thread[] threads = new Thread[threadCount];

        // Делим строки экрана между потоками
        int rowsForOneThread = height / threadCount;

        long startTime = System.currentTimeMillis();

        for (int i = 0; i < threadCount; i++) {

            int startRow = i * rowsForOneThread;
            int endRow = startRow + rowsForOneThread;

            // Последний поток забирает оставшиеся строки
            if (i == threadCount - 1) {
                endRow = height;
            }

            int finalEndRow = endRow;

            threads[i] = new Thread(new Runnable() {
                @Override
                public void run() {

                    for (int y = startRow; y < finalEndRow; y++) {
                        for (int x = 0; x < width; x++) {
                            result[y][x] = isInMandelbrot(x, y);
                        }
                    }
                }
            });

            threads[i].start();
        }

        // Ждём завершения всех потоков
        for (int i = 0; i < threadCount; i++) {
            threads[i].join();
        }

        long endTime = System.currentTimeMillis();

        // Считаем количество пикселей внутри множества
        int pixelsInside = 0;

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                if (result[y][x]) {
                    pixelsInside++;
                }
            }
        }

        System.out.println("Вычисления закончены");
        System.out.println("Количество потоков: " + threadCount);
        System.out.println("Всего пикселей: " + (width * height));
        System.out.println("Пикселей внутри множества: " + pixelsInside);
        System.out.println("Время работы: " + (endTime - startTime) + " мс");
    }

    // Проверяет, входит ли пиксель в множество Мандельброта
    static boolean isInMandelbrot(int pixelX, int pixelY) {

        // Переводим координаты пикселя в обычные координаты
        double cReal = -2.0 + pixelX * 3.0 / width;
        double cImaginary = -1.5 + pixelY * 3.0 / height;

        double zReal = 0;
        double zImaginary = 0;

        for (int i = 0; i < maxIterations; i++) {

            double newReal =
                    zReal * zReal
                            - zImaginary * zImaginary
                            + cReal;

            double newImaginary =
                    2 * zReal * zImaginary
                            + cImaginary;

            zReal = newReal;
            zImaginary = newImaginary;

            // Если число стало слишком большим, то точка точно не входит в множество
            if (zReal * zReal + zImaginary * zImaginary > 4) {
                return false;
            }
        }

        // Если после всех повторений число не стало большим, то считаем, что точка входит в множество
        return true;
    }
}