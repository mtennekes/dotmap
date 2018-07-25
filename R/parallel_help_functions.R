openLog <- function(logfile) {
  f <- file(logfile, open= "at")
  sink(f, append=TRUE)
  sink(f, append=TRUE, type="message")
  f
}
closeLog <- function(f) {
  sink()
  sink(type = "message")
  close(f)
}
