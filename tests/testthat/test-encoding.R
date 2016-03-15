context("encoding")

test_that("Trying to parse the test-encoding.yaml", {
  test_parse <- function(file) {
    class(file) <- get_content_class(file)
    parse_content(file)
  }
  environment(test_parse) <- environment(swirl:::parse_content)
  test_path <- system.file(file.path("test", "test-encoding.yaml"), package = "swirl")
  suppressWarnings(result <- test_parse(test_path))
  console <- capture.output(result)
  wush_path <- system.file(file.path("test", "test-encoding.txt"), package = "swirl")
  wush <- readLines(con = wush_path, warn = FALSE, encoding = "UTF-8")
  expect_equal(strsplit(console[3], "\\s+")[[1]][3], wush)
})
