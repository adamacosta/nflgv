percent_margin <- function(x, y) {
  (x - y) / (x + y)
}

scale_1_10 <- function(x) {
  (x - min(x)) / max(x) * 9 + 1
}