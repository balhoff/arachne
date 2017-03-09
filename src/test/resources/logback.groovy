appender("SystemErr", ConsoleAppender) {
    target = 'System.err'
    encoder(PatternLayoutEncoder) {
        pattern = "%d %level %logger - %msg%n"
    }
}

appender("FILE", FileAppender) {
    file = "rule-engine.log"
    append = true
    encoder(PatternLayoutEncoder) {
        pattern = "%d %level %logger - %msg%n"
    }
}

root(INFO, ["SystemErr", "FILE"])
