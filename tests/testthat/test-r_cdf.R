
test_that("r_cdf() works with named function", {
  set_n(10^5)

  uniform_cdf <- function(x) {
    dplyr::case_when(
      x < 0 ~ 0,
      x > 1 ~ 1,
      T ~ x
    )
  }

  uniform_data <- r_cdf(uniform_cdf, .seed = 6401083)

  expect_equal(
    mean(uniform_data),
    0.5,
    tolerance = 10^-3
  )

  expect_equal(
    median(uniform_data),
    0.5,
    tolerance = 10^-2
  )

  expect_gt(
    min(uniform_data),
    0
  )

  expect_lt(
    max(uniform_data),
    1
  )

  expect_equal(
    sd(uniform_data),
    sqrt(1 / 12),
    tolerance = 10^-2
  )


  exp_cdf <- function(x) {
    dplyr::case_when(
      x < 0 ~ 0,
      T ~ 1 - exp(-x)
    )
  }

  exp_data <- r_cdf(exp_cdf, .seed = 813684)

  expect_equal(
    mean(exp_data),
    1,
    tolerance = 10^-2
  )

  expect_equal(
    median(exp_data),
    log(2),
    tolerance = 10^-2
  )

  expect_gt(
    min(exp_data),
    0
  )

  expect_equal(
    sd(exp_data),
    1,
    tolerance = 10^-2
  )


  logistic_cdf <- function(x) {
    1 / (1 + exp(-x))
  }

  logistic_data <- r_cdf(logistic_cdf, .seed = 458914)

  expect_equal(
    mean(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    median(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    sd(logistic_data),
    sqrt(pi^2 / 3),
    tolerance = 10^-2
  )
})

test_that("r_cdf() works with named function & range", {
  set_n(10^5)

  uniform_cdf <- function(x) x

  uniform_data <- r_cdf(uniform_cdf, min = 0, max = 1, .seed = 448949)

  expect_equal(
    mean(uniform_data),
    0.5,
    tolerance = 10^-3
  )

  expect_equal(
    median(uniform_data),
    0.5,
    tolerance = 10^-2
  )

  expect_gt(
    min(uniform_data),
    0
  )

  expect_lt(
    max(uniform_data),
    1
  )

  expect_equal(
    sd(uniform_data),
    sqrt(1 / 12),
    tolerance = 10^-2
  )


  exp_cdf <- function(x) 1 - exp(-x)


  exp_data <- r_cdf(exp_cdf, min = 0, .seed = 1197894)

  expect_equal(
    mean(exp_data),
    1,
    tolerance = 10^-2
  )

  expect_equal(
    median(exp_data),
    log(2),
    tolerance = 10^-2
  )

  expect_gt(
    min(exp_data),
    0
  )

  expect_equal(
    sd(exp_data),
    1,
    tolerance = 10^-2
  )
})

test_that("r_cdf() works with lambda function .x", {
  set_n(10^5)

  uniform_data <- r_cdf(~.x, min = 0, max = 1, .seed = 997618)

  expect_equal(
    mean(uniform_data),
    0.5,
    tolerance = 10^-2
  )

  expect_equal(
    median(uniform_data),
    0.5,
    tolerance = 10^-2
  )

  expect_gt(
    min(uniform_data),
    0
  )

  expect_lt(
    max(uniform_data),
    1
  )

  expect_equal(
    sd(uniform_data),
    sqrt(1 / 12),
    tolerance = 10^-2
  )


  exp_data <- r_cdf(~ 1 - exp(-.x), min = 0, .seed = 776216)

  expect_equal(
    mean(exp_data),
    1,
    tolerance = 10^-2
  )

  expect_equal(
    median(exp_data),
    log(2),
    tolerance = 10^-2
  )

  expect_gt(
    min(exp_data),
    0
  )

  expect_equal(
    sd(exp_data),
    1,
    tolerance = 10^-2
  )


  logistic_data <- r_cdf(~ 1 / (1 + exp(-.x)), .seed = 35162)

  expect_equal(
    mean(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    median(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    sd(logistic_data),
    sqrt(pi^2 / 3),
    tolerance = 10^-2
  )
})

test_that("r_cdf() works with lambda function .t and argument", {
  set_n(10^5)

  new_variable <- r_lnorm(.seed = 102)

  exp_data <- r_cdf(~ 1 - exp(-beta * .t), min = 0, beta = new_variable, .seed = 993649)


  expect_equal(
    mean(exp_data),
    exp(1 / 2),
    tolerance = 10^-2
  )

  expect_gt(
    min(exp_data),
    0
  )


  logistic_data <- r_cdf(~ 1 / (1 + exp(-beta * .x)), beta = new_variable, .seed = 77632)

  expect_equal(
    mean(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    median(logistic_data),
    0,
    tolerance = 10^-2
  )

  expect_equal(
    sd(logistic_data),
    sqrt(7 * pi^2 / 3),
    tolerance = 10^-2
  )

  df <- tibble::tibble(
    lambda = r_lnorm(n = 100, .seed = 32243),
    k = r_lnorm(n = 100, .seed = 4356)
  )

  weibull_data <- r_cdf(~ 1 - exp(-(.t / lambda)^k), min = 0, data = df, .seed = 4903)

  expect_equal(
    mean(weibull_data),
    7.41622419655323
  )

  expect_equal(
    median(weibull_data),
    0.505118072032928
  )

  expect_equal(
    sd(weibull_data),
    58.1047721185403
  )
})
