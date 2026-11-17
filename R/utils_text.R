# Shorten long messages for table cells.
truncate_text <- function(x, n = 60) {
  x <- ifelse(is.na(x), "", x)
  ifelse(nchar(x) > n, paste0(substr(x, 1, n - 1), "\u2026"), x)
}
