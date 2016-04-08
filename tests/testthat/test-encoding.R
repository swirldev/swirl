context("encoding")

library(stringi)

test_that("Trying to parse the test-encoding.yaml", {
  locale <- Sys.getlocale()
  if(grepl("[L|l]atin", locale)){
    testthat::skip("Locale is Latin")
  }
  
  test_parse <- function(file) {
    class(file) <- get_content_class(file)
    parse_content(file)
  }
  environment(test_parse) <- environment(swirl:::parse_content)
  test_path <- system.file(file.path("test", "test-encoding.yaml"), package = "swirl")
  suppressWarnings(result <- test_parse(test_path))
  console <- capture.output(result)
  test_phrase <- strsplit(console[3], "\\s+")[[1]][3]
  
  if(.Platform$OS.type == "windows"){
    expect_true(
      identical(stri_escape_unicode(test_phrase), "<U+4E2D><U+6587><U+6E2C><U+8A66>") ||
        identical(stri_escape_unicode(test_phrase), stri_escape_unicode("中文測試"))
    )
  } else {
    expect_equal(stri_escape_unicode(test_phrase), stri_escape_unicode("中文測試"))
  }
})
