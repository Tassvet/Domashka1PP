import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/* Многопоточный анализ логов веб-сервера. */
public class LogAnalysisAssignment {

    public record LogEntry(
            String method,
            String path,
            int status,
            long responseBytes,
            long durationMillis
    ) {}

    public record LogFile(Path path) {}

    public record PathStatistics(
            long requests,
            long successful,
            long errors,
            long responseBytes,
            long totalDurationMillis,
            long maxDurationMillis
    ) {}

    public record Report(
            long totalRequests,
            long successfulRequests,
            long errorRequests,
            long responseBytes,
            long maxDurationMillis
    ) {}

    /**
     * Возвращает отдельный итератор для чтения строк одного файла.
     *
     * ВАЖНО: Iterator<String> потоконебезопасен. Один экземпляр нельзя
     * одновременно читать из нескольких потоков. Для каждого потока нужен
     * отдельный вызов LogFileLines.iterator(path).
     */
    public static final class LogFileLines {
        private LogFileLines() {}

        public static FileIterator iterator(Path path) {
            return new FileIterator(path);
        }

        // AutoCloseable нужен, чтобы закрыть файл даже при остановке.
        public static final class FileIterator implements Iterator<String>, AutoCloseable {
            private final BufferedReader reader;
            private final Path path;
            private String nextLine;
            private boolean loaded;
            private boolean closed;

            private FileIterator(Path path) {
                this.path = path;
                try {
                    reader = Files.newBufferedReader(path, StandardCharsets.UTF_8);
                } catch (IOException e) {
                    throw new RuntimeException("Не удалось открыть " + path, e);
                }
            }

            @Override
            public boolean hasNext() {
                if (closed) {
                    return false;
                }
                if (!loaded) {
                    try {
                        nextLine = reader.readLine();
                        loaded = true;
                        if (nextLine == null) {
                            close();
                        }
                    } catch (IOException e) {
                        // Закрытие выполнит try-with-resources в вызывающем коде.
                        throw new RuntimeException("Не удалось прочитать " + path, e);
                    }
                }
                return nextLine != null;
            }

            @Override
            public String next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                String line = nextLine;
                loaded = false;
                nextLine = null;
                return line;
            }

            @Override
            public void close() {
                if (!closed) {
                    closed = true;
                    nextLine = null;
                    try {
                        reader.close();
                    } catch (IOException e) {
                        throw new RuntimeException("Не удалось закрыть " + path, e);
                    }
                }
            }
        }
    }

    /** Генерирует реальные текстовые файлы с детерминированными данными. */
    public static final class GenerateLogs {
        private static final String[] METHODS = {
                "GET", "POST", "PUT", "DELETE"
        };
        private static final String[] PATHS = {
                "/", "/index.html", "/login", "/api/users",
                "/api/orders", "/api/products", "/search", "/health"
        };
        private static final int[] STATUSES = {
                200, 200, 200, 201, 204,
                301, 400, 401, 404, 500
        };

        private GenerateLogs() {}

        public static void generate(
                Path outputDirectory,
                int fileCount,
                int linesPerFile
        ) throws IOException {
            if (fileCount < 0 || linesPerFile < 0) {
                throw new IllegalArgumentException(
                        "Counts must be non-negative"
                );
            }

            Files.createDirectories(outputDirectory);
            Random random = new Random(42);

            for (int fileId = 0; fileId < fileCount; fileId++) {
                Path file = outputDirectory.resolve(
                        "access-%03d.log".formatted(fileId)
                );

                try (BufferedWriter writer = Files.newBufferedWriter(
                        file,
                        StandardCharsets.UTF_8
                )) {
                    for (int line = 0; line < linesPerFile; line++) {
                        String method =
                                METHODS[random.nextInt(METHODS.length)];
                        String path =
                                PATHS[random.nextInt(PATHS.length)];
                        int status =
                                STATUSES[random.nextInt(STATUSES.length)];
                        long responseBytes =
                                100L + random.nextInt(100_000);
                        long durationMillis =
                                1L + random.nextInt(500);

                        writer.write(
                                "%s %s %d %d %d%n".formatted(
                                        method,
                                        path,
                                        status,
                                        responseBytes,
                                        durationMillis
                                )
                        );
                    }
                }
            }
        }
    }

    /** Проверяет генератор и итератор строк. */
    private static void selfCheck() throws IOException {
        Path directory = Files.createTempDirectory("log-assignment-");
        GenerateLogs.generate(directory, 3, 7);

        int files = 0;
        int lines = 0;

        try (var paths = Files.list(directory)) {
            for (Path path : paths.toList()) {
                files++;
                Iterator<String> iterator = LogFileLines.iterator(path);

                while (iterator.hasNext()) {
                    String line = iterator.next();
                    String[] fields = line.trim().split("\\s+");
                    if (fields.length != 5) {
                        throw new AssertionError(
                                "Unexpected line: " + line
                        );
                    }
                    Integer.parseInt(fields[2]);
                    Long.parseLong(fields[3]);
                    Long.parseLong(fields[4]);
                    lines++;
                }
            }
        }

        if (files != 3 || lines != 21) {
            throw new AssertionError(
                    "Expected 3 files and 21 lines, got "
                            + files + " files and " + lines + " lines"
            );
        }

        System.out.println(
                "OK: generated " + files
                        + " files and read " + lines + " lines"
        );
    }

    // Разбираем одну строку. Плохую строку пропускаем, но считаем отдельно.
    public static LogEntry parseLine(String line) {
        if (line == null || line.trim().isEmpty()) {
            throw new IllegalArgumentException("Пустая строка");
        }
        String[] fields = line.trim().split("\\s+");
        if (fields.length != 5) {
            throw new IllegalArgumentException("В строке должно быть 5 полей: " + line);
        }
        int status;
        long bytes;
        long duration;
        try {
            status = Integer.parseInt(fields[2]);
            bytes = Long.parseLong(fields[3]);
            duration = Long.parseLong(fields[4]);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Неверное число в строке: " + line, e);
        }
        // Статусы ниже 200 не относятся ни к успехам, ни к ошибкам по условию.
        // Поэтому такие записи считаем повреждёнными.
        if (status < 200 || bytes < 0 || duration < 0) {
            throw new IllegalArgumentException("Недопустимые значения: " + line);
        }
        return new LogEntry(fields[0], fields[1], status, bytes, duration);
    }

    // Изменяемые данные одного URL. Доступ к ним защищён монитором.
    private static class UrlAccumulator {
        private long requests;
        private long successful;
        private long errors;
        private long bytes;
        private long totalDuration;
        private long maxDuration;

        public synchronized void add(LogEntry entry) {
            requests++;
            if (entry.status() <= 399) {
                successful++;
            } else {
                errors++;
            }
            bytes += entry.responseBytes();
            totalDuration += entry.durationMillis();
            if (entry.durationMillis() > maxDuration) {
                maxDuration = entry.durationMillis();
            }
        }

        public synchronized PathStatistics snapshot() {
            return new PathStatistics(requests, successful, errors, bytes,
                    totalDuration, maxDuration);
        }
    }

    private static class Statistics {
        private final boolean perUrl;
        private final Map<String, UrlAccumulator> globalMap = new HashMap<>();
        private final ConcurrentHashMap<String, UrlAccumulator> urlMap =
                new ConcurrentHashMap<>();

        public Statistics(boolean perUrl) {
            this.perUrl = perUrl;
        }

        public void add(LogEntry entry) {
            if (perUrl) {
                UrlAccumulator accumulator = urlMap.get(entry.path());
                if (accumulator == null) {
                    UrlAccumulator created = new UrlAccumulator();
                    // Если другой поток уже добавил URL, берём его объект.
                    UrlAccumulator previous = urlMap.putIfAbsent(entry.path(), created);
                    accumulator = previous == null ? created : previous;
                }
                accumulator.add(entry);
            } else {
                addWithGlobalLock(entry);
            }
        }

        private synchronized void addWithGlobalLock(LogEntry entry) {
            UrlAccumulator accumulator = globalMap.get(entry.path());
            if (accumulator == null) {
                accumulator = new UrlAccumulator();
                globalMap.put(entry.path(), accumulator);
            }
            accumulator.add(entry);
        }

        // Итоговый снимок вызываем только после завершения всех задач.
        public synchronized Map<String, PathStatistics> snapshot() {
            Map<String, PathStatistics> result = new TreeMap<>();
            Map<String, UrlAccumulator> map = perUrl ? urlMap : globalMap;
            for (Map.Entry<String, UrlAccumulator> item : map.entrySet()) {
                result.put(item.getKey(), item.getValue().snapshot());
            }
            return result;
        }
    }

    // Один объект Analysis используется для одного запуска.
    public static class Analysis {
        private final Statistics statistics;
        private final Semaphore semaphore;
        private volatile boolean stopped;

        private final AtomicInteger processedFiles = new AtomicInteger();
        private final AtomicInteger finishedTasks = new AtomicInteger();
        private final AtomicLong readLines = new AtomicLong();
        private final AtomicLong validEntries = new AtomicLong();
        private final AtomicLong invalidEntries = new AtomicLong();
        private final AtomicInteger readErrors = new AtomicInteger();
        private final AtomicInteger activeFiles = new AtomicInteger();
        private final AtomicInteger maxActiveFiles = new AtomicInteger();
        private final List<String> failedFiles = new ArrayList<>();

        public Analysis(int limit, boolean perUrl) {
            if (limit < 1) {
                throw new IllegalArgumentException("Лимит должен быть больше нуля");
            }
            semaphore = new Semaphore(limit);
            statistics = new Statistics(perUrl);
        }

        public void stop() {
            stopped = true;
        }

        private void updateMaximum(int current) {
            int old = maxActiveFiles.get();
            while (current > old) {
                if (maxActiveFiles.compareAndSet(old, current)) {
                    break;
                }
                old = maxActiveFiles.get();
            }
        }

        private void processFile(LogFile file) {
            boolean acquired = false;
            boolean fullyRead = false;
            try {
                if (stopped) {
                    return;
                }
                // Здесь поток не держит ни одного synchronized-монитора.
                semaphore.acquire();
                acquired = true;
                int current = activeFiles.incrementAndGet();
                updateMaximum(current);
                if (stopped) {
                    return;
                }
                try (LogFileLines.FileIterator iterator = LogFileLines.iterator(file.path())) {
                    while (!stopped) {
                        // Проверка флага выше считается началом операции над строкой.
                        // Если остановка пришла позже, эта операция может завершиться.
                        if (!iterator.hasNext()) {
                            fullyRead = true;
                            break;
                        }
                        String line = iterator.next();
                        readLines.incrementAndGet();
                        LogEntry entry;
                        try {
                            entry = parseLine(line);
                        } catch (IllegalArgumentException e) {
                            invalidEntries.incrementAndGet();
                            continue;
                        }
                        statistics.add(entry);
                        validEntries.incrementAndGet();
                    }
                }
                if (fullyRead) {
                    processedFiles.incrementAndGet();
                }
            } catch (InterruptedException e) {
                stop();
                Thread.currentThread().interrupt();
            } catch (RuntimeException e) {
                if (!(e.getCause() instanceof IOException)) {
                    throw e;
                }
                readErrors.incrementAndGet();
                synchronized (failedFiles) {
                    failedFiles.add(file.path() + ": " + e.getMessage());
                }
            } finally {
                if (acquired) {
                    activeFiles.decrementAndGet();
                    semaphore.release();
                }
                finishedTasks.incrementAndGet();
            }
        }

        public void sequential(List<LogFile> files) {
            for (LogFile file : files) {
                processFile(file);
            }
        }

        public void parallel(List<LogFile> files, int threads) throws InterruptedException {
            if (threads < 1) {
                throw new IllegalArgumentException("Потоков должно быть больше нуля");
            }
            ExecutorService executor = Executors.newFixedThreadPool(threads);
            CountDownLatch latch = new CountDownLatch(files.size());
            boolean interrupted = false;
            try {
                for (LogFile file : files) {
                    executor.execute(() -> {
                        try {
                            processFile(file);
                        } finally {
                            latch.countDown();
                        }
                    });
                }
                try {
                    latch.await();
                } catch (InterruptedException e) {
                    stop();
                    interrupted = true;
                }
            } finally {
                executor.shutdown();
                // Даже если главный поток прервали, сначала закрываем все задачи.
                boolean terminated = false;
                while (!terminated) {
                    try {
                        terminated = executor.awaitTermination(1, TimeUnit.SECONDS);
                    } catch (InterruptedException e) {
                        stop();
                        interrupted = true;
                    }
                }
                if (interrupted) {
                    Thread.currentThread().interrupt();
                }
            }
            if (interrupted) {
                throw new InterruptedException("Анализ остановлен прерыванием");
            }
        }

        public Map<String, PathStatistics> pathReport() {
            return statistics.snapshot();
        }

        public Report report() {
            long requests = 0;
            long successful = 0;
            long errors = 0;
            long bytes = 0;
            long maximum = 0;
            for (PathStatistics item : pathReport().values()) {
                requests += item.requests();
                successful += item.successful();
                errors += item.errors();
                bytes += item.responseBytes();
                maximum = Math.max(maximum, item.maxDurationMillis());
            }
            return new Report(requests, successful, errors, bytes, maximum);
        }

        public void printReport(int fileCount) {
            System.out.println("Общий отчёт: " + report());
            for (Map.Entry<String, PathStatistics> item : pathReport().entrySet()) {
                System.out.println(item.getKey() + " -> " + item.getValue());
            }
            System.out.println("Файлов полностью прочитано: " + processedFiles.get());
            System.out.println("Завершено задач: " + finishedTasks.get() + " / " + fileCount);
            System.out.println("Прочитано строк: " + readLines.get());
            System.out.println("Корректных записей: " + validEntries.get());
            System.out.println("Повреждённых записей: " + invalidEntries.get());
            System.out.println("Ошибок чтения файлов: " + readErrors.get());
            System.out.println("Активных файлов сейчас: " + activeFiles.get());
            System.out.println("Максимум активных файлов: " + maxActiveFiles.get());
            System.out.println("Была запрошена остановка: " + stopped);
            if (stopped || readErrors.get() > 0 || invalidEntries.get() > 0) {
                System.out.println("Отчёт частичный: учтены только прочитанные корректные записи.");
            }
            synchronized (failedFiles) {
                for (String file : failedFiles) {
                    System.out.println("Ошибка файла: " + file);
                }
            }
        }
    }

    private static List<LogFile> findFiles(Path directory) throws IOException {
        List<LogFile> result = new ArrayList<>();
        try (var paths = Files.newDirectoryStream(directory, "*.log")) {
            for (Path path : paths) {
                // Даже недоступный файл должен попасть в анализ и в список ошибок.
                result.add(new LogFile(path));
            }
        }
        return result;
    }

    private static void require(boolean condition, String message) {
        if (!condition) {
            throw new AssertionError(message);
        }
    }

    private static void checkInvariants(Analysis analysis, int limit, int files) {
        require(analysis.activeFiles.get() == 0, "Остались активные файлы");
        require(analysis.maxActiveFiles.get() <= limit, "Нарушен лимит семафора");
        require(analysis.maxActiveFiles.get() >= 0, "Отрицательный максимум");
        require(analysis.finishedTasks.get() == files, "Не все задачи завершены");
        require(analysis.readLines.get() == analysis.validEntries.get()
                + analysis.invalidEntries.get(), "Неверное число строк");
        require(analysis.report().totalRequests() == analysis.validEntries.get(),
                "Потерялись корректные записи");
        for (PathStatistics item : analysis.pathReport().values()) {
            require(item.requests() == item.successful() + item.errors(),
                    "Неверное число запросов");
            require(item.maxDurationMillis() <= item.totalDurationMillis(),
                    "Максимум больше суммы");
        }
    }

    private static void solutionCheck() throws Exception {
        LogEntry entry = parseLine("  GET   /test 399 10 0  ");
        require(entry.status() == 399 && entry.responseBytes() == 10, "Ошибка разбора");
        String[] badLines = {"", "GET / 200 1", "GET / abc 1 1",
                "GET / 200 -1 1", "GET / 199 1 1", "GET / 200 1 -1",
                "GET / 200 9999999999999999999999999 1"};
        for (String line : badLines) {
            boolean rejected = false;
            try {
                parseLine(line);
            } catch (IllegalArgumentException e) {
                rejected = true;
            }
            require(rejected, "Принята повреждённая строка: " + line);
        }
        Path directory = Files.createTempDirectory("log-solution-check-");
        GenerateLogs.generate(directory, 8, 2000);
        List<LogFile> files = findFiles(directory);
        Analysis sequential = new Analysis(1, false);
        sequential.sequential(files);
        checkInvariants(sequential, 1, files.size());
        require(sequential.report().totalRequests() == 16000, "Потеря строк");
        for (int threads : new int[]{1, 4}) {
            for (int limit : new int[]{1, 4}) {
                for (boolean perUrl : new boolean[]{false, true}) {
                    Analysis parallel = new Analysis(limit, perUrl);
                    parallel.parallel(files, threads);
                    require(sequential.pathReport().equals(parallel.pathReport()),
                            "Последовательный и параллельный отчёты не совпадают");
                    require(sequential.report().equals(parallel.report()), "Итоги не совпадают");
                    require(parallel.processedFiles.get() == 8, "Файлы не дочитаны");
                    checkInvariants(parallel, limit, files.size());
                }
            }
        }
        // Простой пример с заранее известным ответом.
        Path broken = directory.resolve("broken.log");
        Files.writeString(broken, "GET /test 200 10 3\nплохая строка\n"
                + "POST /test 400 20 7\nGET /test 399 5 0\n");
        List<LogFile> damaged = new ArrayList<>();
        damaged.add(new LogFile(broken));
        damaged.add(new LogFile(directory.resolve("missing.log")));
        Path empty = directory.resolve("empty.log");
        Files.writeString(empty, "");
        damaged.add(new LogFile(empty));
        Path unreadable = directory.resolve("invalid-utf8.log");
        Files.write(unreadable, new byte[]{(byte) 0xC3, (byte) 0x28});
        damaged.add(new LogFile(unreadable));
        Analysis partial = new Analysis(2, true);
        partial.parallel(damaged, 4);
        require(partial.report().equals(new Report(3, 2, 1, 35, 7)), "Ошибка частичного отчёта");
        require(partial.pathReport().get("/test").totalDurationMillis() == 10,
                "Ошибка суммы времени");
        require(partial.invalidEntries.get() == 1, "Не замечена повреждённая строка");
        require(partial.readErrors.get() == 2 && partial.failedFiles.size() == 2,
                "Не замечена ошибка файла");
        require(partial.processedFiles.get() == 2, "Ошибка счётчика файлов");
        checkInvariants(partial, 2, damaged.size());
        Analysis stopped = new Analysis(1, true);
        stopped.stop();
        stopped.parallel(files, 4);
        require(stopped.readLines.get() == 0, "Остановка до запуска не работает");
        checkInvariants(stopped, 1, files.size());
        Analysis emptyAnalysis = new Analysis(1, true);
        emptyAnalysis.parallel(new ArrayList<>(), 4);
        checkInvariants(emptyAnalysis, 1, 0);
        System.out.println("OK: разбор, отчёты, 8 вариантов настроек, ошибки и остановка до запуска");
    }

    // Экспериментальные данные: все строки с одним URL или с 2000 разными URL.
    private static void generateExperiment(Path directory, boolean manyUrls) throws IOException {
        Files.createDirectories(directory);
        for (int file = 0; file < 8; file++) {
            try (BufferedWriter writer = Files.newBufferedWriter(directory.resolve(file + ".log"))) {
                for (int line = 0; line < 10000; line++) {
                    String path = manyUrls ? "/page/" + (line % 2000) : "/same";
                    writer.write("GET " + path + " 200 100 5");
                    writer.newLine();
                }
            }
        }
    }

    private static void experiments() throws Exception {
        Path directory = Files.createTempDirectory("log-experiment-");
        for (boolean manyUrls : new boolean[]{false, true}) {
            Path data = directory.resolve(manyUrls ? "many" : "one");
            generateExperiment(data, manyUrls);
            List<LogFile> files = findFiles(data);
            Analysis reference = new Analysis(1, false);
            reference.sequential(files);
            System.out.println(manyUrls ? "2000 разных URL" : "Один URL");
            System.out.println("Потоки; лимит; блокировка; время, мс; максимум файлов");
            for (int threads : new int[]{1, 4}) {
                for (int limit : new int[]{1, 4}) {
                    for (boolean perUrl : new boolean[]{false, true}) {
                        Analysis analysis = new Analysis(limit, perUrl);
                        long start = System.nanoTime();
                        analysis.parallel(files, threads);
                        long millis = (System.nanoTime() - start) / 1_000_000;
                        require(reference.pathReport().equals(analysis.pathReport()),
                                "Эксперимент изменил результат");
                        checkInvariants(analysis, limit, files.size());
                        System.out.println(threads + "; " + limit + "; "
                                + (perUrl ? "по URL" : "общая") + "; " + millis
                                + "; " + analysis.maxActiveFiles.get());
                    }
                }
            }
        }
        System.out.println("Время зависит от компьютера, прогрева Java и дискового кэша.");
    }

    public static void main(String[] args) throws Exception {
        if (args.length == 0) {
            System.out.println("Нужна Java 17 или новее.");
            System.out.println("java LogAnalysisAssignment.java generate logs 20 1000");
            System.out.println("java LogAnalysisAssignment.java check");
            System.out.println("java LogAnalysisAssignment.java sequential logs");
            System.out.println("java LogAnalysisAssignment.java parallel logs 4 2 url");
            System.out.println("java LogAnalysisAssignment.java parallel logs 4 2 global");
            System.out.println("java LogAnalysisAssignment.java stop logs 4 2 url 10");
            System.out.println("java LogAnalysisAssignment.java experiments");
            return;
        }
        if (args[0].equals("generate")) {
            Path directory = Path.of(args.length > 1 ? args[1] : "logs");
            int count = args.length > 2 ? Integer.parseInt(args[2]) : 20;
            int lines = args.length > 3 ? Integer.parseInt(args[3]) : 1000;
            GenerateLogs.generate(directory, count, lines);
            System.out.println("Создано файлов: " + count + ", каталог: " + directory);
            return;
        }
        if (args[0].equals("check")) {
            selfCheck();
            solutionCheck();
            return;
        }
        if (args[0].equals("experiments")) {
            experiments();
            return;
        }
        boolean sequential = args[0].equals("sequential");
        boolean stopDemo = args[0].equals("stop");
        if (!sequential && !stopDemo && !args[0].equals("parallel")) {
            throw new IllegalArgumentException("Неизвестная команда: " + args[0]);
        }
        Path directory = Path.of(args.length > 1 ? args[1] : "logs");
        int threads = args.length > 2 ? Integer.parseInt(args[2]) : 4;
        int limit = args.length > 3 ? Integer.parseInt(args[3]) : 2;
        String mode = args.length > 4 ? args[4] : "url";
        if (!mode.equals("url") && !mode.equals("global")) {
            throw new IllegalArgumentException("Режим: url или global");
        }
        List<LogFile> files = findFiles(directory);
        Analysis analysis = new Analysis(sequential ? 1 : limit, mode.equals("url"));
        Thread stopper = null;
        if (stopDemo) {
            long delay = args.length > 5 ? Long.parseLong(args[5]) : 10;
            if (delay < 0) {
                throw new IllegalArgumentException("Задержка не может быть отрицательной");
            }
            stopper = new Thread(() -> {
                try {
                    Thread.sleep(delay);
                    analysis.stop();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            });
            stopper.start();
        }
        long start = System.nanoTime();
        try {
            if (sequential) {
                analysis.sequential(files);
            } else {
                analysis.parallel(files, threads);
            }
        } finally {
            if (stopper != null) {
                stopper.interrupt();
                stopper.join();
            }
        }
        long millis = (System.nanoTime() - start) / 1_000_000;
        checkInvariants(analysis, sequential ? 1 : limit, files.size());
        analysis.printReport(files.size());
        System.out.println("Время анализа, мс: " + millis);
    }

    /*
     * ПОЯСНЕНИЕ К РЕШЕНИЮ
     *
     * Компиляция:
     * javac -encoding UTF-8 LogAnalysisAssignment.java
     * и запускать java LogAnalysisAssignment ... (без .java).
     *
     * Повреждённая строка: пропускаем только её. Остальные строки файла читаем.
     * Статус < 200 и отрицательные байты/время считаем повреждением.
     * Статусы >= 400 считаем ошибками без дополнительной верхней границы,
     * потому что именно так написано в условии.
     * Пустая строка тоже считается повреждённой.
     *
     * Ошибка открытия/чтения/закрытия: записываем файл в failedFiles, увеличиваем
     * readErrors. Уже учтённые строки сохраняем, остальные файлы продолжаем.
     * processedFiles — файлы, прочитанные до конца и успешно закрытые.
     * Файл с плохой строкой тоже может быть прочитан до конца.
     * finishedTasks — задачи, которые закончились, включая ошибки и остановку.
     * readLines — строки, полученные итератором и переданные разбору.
     * Все общие счётчики Atomic. Обычные long внутри UrlAccumulator защищены
     * synchronized и являются статистикой URL, а не глобальными счётчиками.
     *
     * Частичный отчёт после stop(): только корректные записи из начатых операций,
     * которые успели завершиться. Файл может быть прочитан частично или не открыт.
     * Операция начинается с проверки stopped в цикле. Если флаг изменился сразу
     * после проверки, эта операция всё ещё имеет право завершиться.
     * volatile делает новое значение флага видимым другим потокам.
     * Без volatile поток мог бы продолжать видеть старое значение.
     * stop() не обрывает уже выполняющееся чтение. Для обычных локальных файлов
     * ждём его окончания; мгновенную отмену зависшего диска программа не обещает.
     * Итоговые методы report/pathReport/printReport вызываем после завершения
     * sequential/parallel. Во время работы целостный снимок не обещается.
     *
     * ДОКАЗАТЕЛЬСТВА ИНВАРИАНТОВ
     * 1. В полном запуске без ошибок чтения каждый элемент списка файлов имеет
     * одну задачу и один собственный итератор. Цикл получает строку один раз.
     * Корректная строка один раз добавляется в статистику, плохая один раз
     * увеличивает invalidEntries. Список из findFiles не содержит повторов.
     * При ручном создании списка нужно тоже не добавлять один файл дважды.
     * Если чтение оборвалось, обещать обработку непрочитанных строк нельзя.
     *
     * 2. В add одновременно увеличивается requests и ровно один из счётчиков
     * successful/errors. Весь метод защищён монитором, поэтому изменения
     * двух потоков не теряются. Отсюда requests = successful + errors.
     *
     * 3. Время неотрицательно, поэтому его максимум не больше суммы.
     * Предполагаем, что суммы помещаются в long, как и в структурах задания.
     *
     * 4. activeFiles увеличивается только после acquire. До release счётчик
     * уменьшается. Одно разрешение учитывает не более одного активного файла.
     * Поэтому 0 <= activeFiles <= limit. Каждый получивший разрешение поток
     * проходит finally: после завершения всех задач activeFiles = 0.
     * CAS меняет максимум только при совпадении со старым значением; при гонке
     * повторяем попытку. Поэтому значение максимума не теряется.
     *
     * 5. Ошибки ввода-вывода перехватываются внутри processFile. finally
     * возвращает разрешение, а finally задачи уменьшает latch. Остальные
     * задачи не отменяются, и ошибка видна в списке файлов и счётчике.
     *
     * 6. Порядок захвата: сначала Semaphore, потом блокировка статистики.
     * В global: монитор Statistics, затем монитор UrlAccumulator.
     * В url: операция ConcurrentHashMap заканчивается до входа в монитор URL.
     * Ни один монитор URL не захватывает другой монитор URL или Statistics.
     * acquire никогда не вызывается под synchronized. failedFiles имеет
     * отдельный короткий блок, внутри него другие блокировки не берём.
     * Обратного порядка нет, значит нет цикла ожидания блокировок.
     * Главный поток ждёт latch без мониторов и разрешений. countDown всегда
     * стоит в finally. Поэтому дедлока из-за блокировок и ожидания задач нет.
     * При ошибке и остановке close и release выполняются через finally.
     * Это доказательство для нормальной работы JVM и завершающихся операций
     * ввода-вывода, без аварии JVM и исчерпания памяти.
     *
     * ЭКСПЕРИМЕНТЫ
     * check сравнивает эталон и 8 настроек потоков/лимита/блокировок,
     * затем проверяет плохую строку, отсутствующий файл, неверный UTF-8,
     * пустой файл, пустой список и остановку до запуска.
     * experiments создаёт по 80000 строк для одного и 2000 URL, печатает время
     * всех 8 настроек и проверяет совпадение каждого отчёта с эталоном.
     * stop позволяет отдельно посмотреть остановку во время обработки.
     * Лимит 1 не даёт читать два файла одновременно даже при 4 потоках.
     * Для одного URL его монитор всё равно общий для всех запросов.
     * Для разных URL отдельные мониторы позволяют обновлять статистику
     * параллельно. Это не гарантирует ускорение: есть чтение, разбор строк,
     * Atomic-счётчики, работа JVM. Для сравнения времени полезно несколько
     * запусков; один замер сам по себе не доказывает преимущество.
     *
     * РЕЗУЛЬТАТ ОДНОГО ЗАПУСКА ЭКСПЕРИМЕНТОВ (Java 17, время в мс)
     * Потоки  Лимит  Блокировка   Один URL  2000 URL
     *   1      1    общая          110       89
     *   1      1    по URL          78       72
     *   1      4    общая           77       45
     *   1      4    по URL          52       51
     *   4      1    общая           58       45
     *   4      1    по URL          81       59
     *   4      4    общая           38       31
     *   4      4    по URL          32       35
     * Во всех строках отчёт совпал с последовательным.
     * При одном потоке или лимите 1 максимум активных файлов был 1.
     * При четырёх потоках и лимите 4 максимум был 4.
     * В этом замере отдельные блокировки для 2000 URL не дали ускорения
     * относительно общей блокировки при 4 потоках и лимите 4: 35 против 31 мс.
     * На таких небольших данных разница мала и зависит от условий запуска.
     *
     * Дополнительно проверена остановка во время работы на 200000 строках:
     * stop() дал частичный отчёт на 102 строки; прерывание главного потока —
     * на 126 строк. В обоих случаях завершились все 20 задач, activeFiles = 0.
     * Число строк при остановке не фиксировано: оно зависит от момента вызова.
     */
}
