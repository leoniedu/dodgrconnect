# Helper function to generate random hash strings
genhash <- function (len = 10) {
  paste0 (sample (c (letters, LETTERS, 0:9), size = len, replace = TRUE),
          collapse = "")
}
