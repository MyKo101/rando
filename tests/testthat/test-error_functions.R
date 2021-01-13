
test_that("check_must_be_positive() throws correct errors", {
  expect_error(
    check_must_be_positive(-1)
  )

  expect_error(
    check_must_be_positive(-100:-50)
  )

  expect_error(
    check_must_be_positive(c(-1, 1:100))
  )
})

test_that("check_must_be_strictly_positive() throws correct error", {
  expect_error(
    check_must_be_strictly_positive(-1)
  )

  expect_error(
    check_must_be_strictly_positive(-100:-50)
  )

  expect_error(
    check_must_be_strictly_positive(c(0, 1:100))
  )
})

test_that("check_must_be_between() throws correct error", {
  expect_error(
    check_must_be_between(-1, 0, 1)
  )


  expect_error(
    check_must_be_between(-100:-50, -49, 100)
  )

  expect_error(
    check_must_be_between(1:100, 29, 70)
  )
})


test_that("check_must_be_strictly_between() throws correct error", {
  expect_error(
    check_must_be_strictly_between(-1, 0, 1)
  )


  expect_error(
    check_must_be_strictly_between(-100:-50, -50, 100)
  )

  expect_error(
    check_must_be_strictly_between(1:100, 29, 70)
  )
})


test_that("check_must_be_integer() throws correct error", {
  expect_error(
    check_must_be_integer(1.5)
  )


  expect_error(
    check_must_be_integer(c(0.2, 1:100))
  )

  expect_error(
    check_must_be_integer(seq(0, 1, 0.5))
  )
})



test_that("check_n() throws correct error", {
  expect_error(
    check_n(1.5)
  )


  expect_error(
    check_n(1:2)
  )

  expect_error(
    check_n(-1)
  )
})
