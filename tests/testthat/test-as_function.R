
test_that("as_function() produces functions", {
  expect_type(
    as_function(~ 1 + .x),
    "closure"
  )

  expect_type(
    as_function(mean),
    "closure"
  )

  expect_equal(
    as_function(mean),
    mean
  )

  expect_equal(
    as_function(`+`),
    `+`
  )

  expect_s3_class(
    as_function(~ 1 + .x),
    "function"
  )

  expect_s3_class(
    as_function(~ 1 + .x),
    "rlang_lambda_function"
  )
  expect_s3_class(
    as_function(~ 1 + .x),
    "rando_lambda_function"
  )
})

test_that("as_function() acts like rlang's", {
  expect_equal(
    as_function(~ 1 + .x)(1:10),
    rlang::as_function(~ 1 + .x)(1:10),
  )

  expect_equal(
    as_function(~ .y * (32 + .x^2))(1:10, 5),
    rlang::as_function(~ .y * (32 + .x^2))(1:10, 5),
  )

  expect_equal(
    as_function(~ sin(.x) * cos(.y))(1:100, 1:100),
    rlang::as_function(~ sin(.x) * cos(.y))(1:100, 1:100)
  )
})

test_that("as_function() works on .t", {
  expect_equal(
    as_function(~ 1 + .t)(1:10),
    rlang::as_function(~ 1 + .x)(1:10),
  )

  expect_equal(
    as_function(~ .y * (32 + .t^2))(1:10, 5),
    rlang::as_function(~ .y * (32 + .x^2))(1:10, 5),
  )

  expect_equal(
    as_function(~ sin(.t) * cos(.y))(1:100, 1:100),
    rlang::as_function(~ sin(.x) * cos(.y))(1:100, 1:100)
  )
})

test_that("as_function() takes additional arguments", {
  expect_equal(
    as_function(~ alpha + .t)(1:10, alpha = 1),
    rlang::as_function(~ 1 + .x)(1:10),
  )

  expect_equal(
    as_function(~ .y * (alpha + .t^2))(1:10, 5, alpha = 32),
    rlang::as_function(~ .y * (32 + .x^2))(1:10, 5),
  )

  expect_equal(
    as_function(~ f1(.t) * f2(.y))(1:100, 1:100, f1 = sin, f2 = cos),
    rlang::as_function(~ sin(.x) * cos(.y))(1:100, 1:100)
  )
})
