
test_that("is_wholenumber() returns correctly", {
  expect_true(
    is_wholenumber(1L)
  )

  expect_true(
    is_wholenumber(5)
  )

  expect_false(
    is_wholenumber(1.5)
  )

  expect_false(
    is_wholenumber("a")
  )

  expect_false(
    all(is_wholenumber(letters))
  )
  expect_length(
    is_wholenumber(letters),
    26
  )

  expect_true(
    all(is_wholenumber(1:10))
  )

  expect_length(
    is_wholenumber(1:10),
    10
  )

  expect_false(
    all(is_wholenumber(c(0.5, 0.4, 0.3)))
  )

  expect_length(
    is_wholenumber(c(0.5, 0.4, 0.3)),
    3
  )

  expect_equal(
    is_wholenumber(c(1, 2, 5, 2.1, -3, pi)),
    c(T, T, T, F, T, F)
  )
})

test_that("match.call2() returns correctly", {
  f1 <- function(n) {
    f2(n)
  }
  f2 <- function(n) {
    f3(n)
  }
  f3 <- function(n) {
    f4(n)
  }
  f4 <- function(n) {
    match.call2(n)
  }

  expect_equal(
    f1(0),
    quote(f4(n = n))
  )

  expect_equal(
    f1(1),
    quote(f3(n = n))
  )

  expect_equal(
    f1(2),
    quote(f2(n = n))
  )

  expect_equal(
    f1(3),
    quote(f1(n = 3))
  )
})

test_that("null_switch() returns correctly", {
  f_null <- function() NULL
  f_1 <- function() 1
  f_cat_null <- function() {
    cat("Output")
    NULL
  }
  f_cat_2 <- function() {
    cat("Output")
    2
  }

  expect_null(
    null_switch()
  )

  expect_null(
    null_switch(NULL)
  )

  expect_null(
    null_switch(f_null(), NULL)
  )

  expect_null(
    null_switch(NULL, f_null())
  )

  expect_null(
    null_switch(NULL, f_null())
  )

  expect_output(
    null_switch(f_cat_null(), NULL),
    "Output"
  )

  expect_equal(
    null_switch(f_1(), NULL),
    1
  )

  expect_output(
    null_switch(f_cat_2(), NULL),
    "Output"
  )

  expect_equal(
    null_switch(f_cat_2(), NULL),
    2
  )

  expect_silent(
    null_switch(f_1(), f_cat_null())
  )

  expect_silent(
    null_switch(NULL, f_null(), f_1(), f_cat_2())
  )
})
