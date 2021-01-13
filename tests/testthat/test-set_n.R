

test_that("set_n() works", {

  # Should return n invisibly
  expect_equal(
    set_n(30),
    30
  )

  expect_invisible(set_n(30))

  # And returned value should be n
  expect_equal(
    getOption("rando.n"),
    30
  )

  expect_equal(
    set_n(NULL),
    NULL
  )
})

test_that("set_n() throws errors when n is unfeasible", {
  expect_error(set_n(-1))
  expect_error(set_n(0.5))
  expect_error(set_n(1:2))
})

test_that("get_n() retrieves value from set_n()", {
  expect_equal(
    get_n(),
    getOption("rando.n")
  )

  expect_equal(
    set_n(10),
    get_n()
  )

  expect_equal(
    set_n(100),
    get_n()
  )
})

set_n(100)
df <- tibble::tibble(id = 1:25)

test_that("default_n() extracts the correct values", {
  set_n(NULL)
  expect_equal(
    default_n(),
    1
  )
  set_n(100)

  expect_equal(
    default_n(),
    100
  )

  expect_equal(
    default_n(x = 1:10),
    10
  )

  expect_equal(
    tibble::tibble(id = 1:5, n = default_n())$n[1],
    5
  )

  expect_equal(
    dplyr::mutate(df, n = default_n())$n[1],
    25
  )
})

test_that("tibble_n() extracts the correct value", {
  expect_null(
    tibble_n()
  )

  expect_equal(
    tibble::tibble(id = 1:5, n = tibble_n())$n[1],
    5
  )

  expect_equal(
    tibble::tibble(.rows = 100, n = tibble_n())$n[1],
    100
  )

  expect_equal(
    tibble::tibble(id = 1:15, n = 3 + tibble_n())$n[1],
    18
  )

  expect_equal(
    tibble::tibble(id = 1:10, n = 1:tibble_n())$n,
    1:10
  )

  expect_null(
    tibble::tibble(id = 1, n = list(tibble_n()))$n[[1]]
  )
})

test_that("dplyr_n() extracts the correct value", {
  expect_null(
    dplyr_n()
  )

  expect_equal(
    dplyr::mutate(df, n = dplyr_n())$n[1],
    25
  )

  expect_equal(
    dplyr::summarise(df, n = dplyr_n())$n,
    25
  )
})

test_that("args_n() extracts the correct value", {
  expect_equal(
    args_n(),
    1
  )

  expect_equal(
    args_n(NULL),
    1
  )

  expect_equal(
    args_n(1:10),
    10
  )

  expect_equal(
    args_n(1:30),
    30
  )

  expect_equal(
    args_n(c("a", "b", "c")),
    3
  )

  expect_equal(
    args_n(1:3, 1:4),
    c(3, 4)
  )

  expect_equal(
    args_n(c("a", "b", "c"), 1:3, NULL, 1),
    c(3, 3, 1)
  )
})
