
test_that("function can use dots", {
  f_a_old <- function(...) {
    a
  }

  f_a <- function(...) {
    extract_dots()
    a
  }

  f_ab <- function(...) {
    extract_dots()
    a * b
  }

  f_ls <- function(...) {
    extract_dots()
    ls()
  }

  expect_error(
    f_a_old(a = 12)
  )

  expect_equal(
    f_a(a = 12),
    12
  )
  expect_equal(
    f_a(a = "hello"),
    "hello"
  )
  expect_equal(
    f_ab(a = 1:3, b = 2:4),
    c(2, 6, 12)
  )

  expect_equal(
    f_ls(a = 1, b = "test", c = NA, d = NULL),
    c("a", "b", "c", "d")
  )
})
