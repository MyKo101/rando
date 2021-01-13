set_n(100)
df <- tibble(id = 1:25)

#### r_norm() ####

test_that("r_norm() lengths work", {

  # Check we're getting the default length
  expect_length(r_norm(), 100)

  # Check we can overwrite the length manually
  expect_length(r_norm(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_norm(mean = c(-10, 10)), 2)
  expect_length(r_norm(sd = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_norm())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_norm())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_norm())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_norm())$x)), 1)
})

test_that("r_norm() errors work", {
  expect_error(r_norm(sd = -1))
  expect_error(r_norm(sd = 0))

  expect_error(r_norm(mean = c(1, 2, 3), sd = c(0.5, 1)))

  expect_error(r_norm(n = -1))
  expect_error(r_norm(n = c(1, 2)))
  expect_error(r_norm(n = 1.5))

  expect_error(r_norm(.seed = -2^31))
  expect_error(r_norm(.seed = 2^31))
  expect_error(r_norm(.seed = FALSE))
  expect_error(r_norm(.seed = "error"))
})

test_that("r_norm() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_norm(.seed = 100)), 100)
  expect_null(pull_seed(r_norm()))
  expect_false(is.null(pull_seed(r_norm(.seed = T))))
})

test_that("r_norm() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_norm(.seed = 123456),
    {
      set.seed(123456)
      stats::rnorm(100)
    },
    check.attributes = F
  )

  expect_equal(
    r_norm(10, 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rnorm(100, 10, 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_norm(c(10, 23, 100, 91), 4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rnorm(4, c(10, 23, 100, 91), 4)
    },
    check.attributes = F
  )
})

#### r_beta() ####

test_that("r_beta() lengths work", {

  # Check we're getting the default length
  expect_length(r_beta(1, 1), 100)

  # Check we can overwrite the length manually
  expect_length(r_beta(1, 1, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_beta(alpha = c(1, 2), 1), 2)
  expect_length(r_beta(1, beta = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_beta(1, 1))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_beta(1, 1))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_beta(1, 1))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_beta(1, 1))$x)), 1)
})

test_that("r_beta() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_beta(alpha = -1, 1))
  expect_error(r_beta(alpha = 0, 1))

  expect_error(r_beta(1, beta = -1))
  expect_error(r_beta(1, beta = 0))

  expect_error(r_beta(alpha = c(1, 2, 3), beta = c(0.5, 1)))

  expect_error(r_beta(1, 1, n = -1))
  expect_error(r_beta(1, 1, n = c(1, 2)))
  expect_error(r_beta(1, 1, n = 1.5))

  expect_error(r_beta(.seed = -2^31))
  expect_error(r_beta(.seed = 2^31))
  expect_error(r_beta(.seed = FALSE))
  expect_error(r_beta(.seed = "error"))
})

test_that("r_beta() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_beta(1, 1, .seed = 100)), 100)
  expect_null(pull_seed(r_beta(1, 1)))
  expect_false(is.null(pull_seed(r_beta(1, 1, .seed = T))))
})

test_that("r_beta() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_beta(1, 1, .seed = 123456),
    {
      set.seed(123456)
      stats::rbeta(100, 1, 1)
    },
    check.attributes = F
  )

  expect_equal(
    r_beta(10, 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rbeta(100, 10, 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_beta(c(10, 23, 100, 91), 4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rbeta(4, c(10, 23, 100, 91), 4)
    },
    check.attributes = F
  )
})

#### r_binom() ####

test_that("r_binom() lengths work", {

  # Check we're getting the default length
  expect_length(r_binom(10), 100)

  # Check we can overwrite the length manually
  expect_length(r_binom(10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_binom(size = c(1, 2)), 2)
  expect_length(r_binom(1, prob = c(0.2, 0.5)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_binom(10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_binom(10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_binom(10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_binom(10))$x)), 1)
})

test_that("r_binom() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_binom(10, prob = -1))
  expect_error(r_binom(10, prob = 1.5))

  expect_error(r_binom(size = 1.5))
  expect_error(r_binom(size = -1))

  expect_error(r_binom(size = c(1, 2, 3), prob = c(0.2, 0.3)))

  expect_error(r_binom(10, n = -1))
  expect_error(r_binom(10, n = c(1, 2)))
  expect_error(r_binom(10, n = 1.5))

  expect_error(r_binom(10, .seed = -2^31))
  expect_error(r_binom(10, .seed = 2^31))
  expect_error(r_binom(10, .seed = FALSE))
  expect_error(r_binom(10, .seed = "error"))
})

test_that("r_binom() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_binom(10, .seed = 100)), 100)
  expect_null(pull_seed(r_binom(10)))
  expect_false(is.null(pull_seed(r_binom(10, .seed = T))))
})

test_that("r_binom() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_binom(10, .seed = 123456),
    {
      set.seed(123456)
      stats::rbinom(100, 10, 0.5)
    },
    check.attributes = F
  )

  expect_equal(
    r_binom(10, 0.2, .seed = 9876),
    {
      set.seed(9876)
      stats::rbinom(100, 10, 0.2)
    },
    check.attributes = F
  )

  expect_equal(
    r_binom(c(10, 23, 100, 91), 0.4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rbinom(4, c(10, 23, 100, 91), 0.4)
    },
    check.attributes = F
  )
})

#### r_cauchy() ####

test_that("r_cauchy() lengths work", {

  # Check we're getting the default length
  expect_length(r_cauchy(), 100)

  # Check we can overwrite the length manually
  expect_length(r_cauchy(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_cauchy(location = c(-1, 1)), 2)
  expect_length(r_cauchy(scale = c(2, 3)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_cauchy())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_cauchy())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_cauchy())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_cauchy())$x)), 1)
})

test_that("r_cauchy() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_cauchy(scale = -1))
  expect_error(r_cauchy(scale = 0))

  expect_error(loction = c(1, 2, 3), scale = c(1, 2))

  expect_error(r_cauchy(n = -1))
  expect_error(r_cauchy(n = c(1, 2)))
  expect_error(r_cauchy(n = 1.5))

  expect_error(r_cauchy(.seed = -2^31))
  expect_error(r_cauchy(.seed = 2^31))
  expect_error(r_cauchy(.seed = FALSE))
  expect_error(r_cauchy(.seed = "error"))
})

test_that("r_cauchy() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_cauchy(.seed = 100)), 100)
  expect_null(pull_seed(r_cauchy()))
  expect_false(is.null(pull_seed(r_cauchy(.seed = T))))
})

test_that("r_cauchy() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_cauchy(.seed = 123456),
    {
      set.seed(123456)
      stats::rcauchy(100)
    },
    check.attributes = F
  )

  expect_equal(
    r_cauchy(10, 0.2, .seed = 9876),
    {
      set.seed(9876)
      stats::rcauchy(100, 10, 0.2)
    },
    check.attributes = F
  )

  expect_equal(
    r_cauchy(c(10, 23, 100, 91), 0.4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rcauchy(4, c(10, 23, 100, 91), 0.4)
    },
    check.attributes = F
  )
})

#### r_chisw() ####

test_that("r_chisq() lengths work", {

  # Check we're getting the default length
  expect_length(r_chisq(10), 100)

  # Check we can overwrite the length manually
  expect_length(r_chisq(10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_chisq(df = c(10, 20)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_chisq(10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_chisq(10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_chisq(10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_chisq(10))$x)), 1)
})

test_that("r_chisq() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_chisq(df = -1))
  expect_error(r_chisq(df = 0))

  expect_error(r_chisq(10, n = -1))
  expect_error(r_chisq(10, n = c(1, 2)))
  expect_error(r_chisq(10, n = 1.5))

  expect_error(r_chisq(10, .seed = -2^31))
  expect_error(r_chisq(10, .seed = 2^31))
  expect_error(r_chisq(10, .seed = FALSE))
  expect_error(r_chisq(10, .seed = "error"))
})

test_that("r_chisq() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_chisq(10, .seed = 100)), 100)
  expect_null(pull_seed(r_chisq(10)))
  expect_false(is.null(pull_seed(r_chisq(10, .seed = T))))
})

test_that("r_chisq() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_chisq(10, .seed = 123456),
    {
      set.seed(123456)
      stats::rchisq(100, 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_chisq(c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rchisq(4, c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_exp() ####

test_that("r_exp() lengths work", {

  # Check we're getting the default length
  expect_length(r_exp(), 100)

  # Check we can overwrite the length manually
  expect_length(r_exp(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_exp(rate = c(1 / 100, 1 / 50)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_exp())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_exp())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_exp())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_exp())$x)), 1)
})

test_that("r_exp() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_exp(rate = -1))
  expect_error(r_exp(rate = 0))

  expect_error(r_exp(n = -1))
  expect_error(r_exp(n = c(1, 2)))
  expect_error(r_exp(n = 1.5))

  expect_error(r_exp(.seed = -2^31))
  expect_error(r_exp(.seed = 2^31))
  expect_error(r_exp(.seed = FALSE))
  expect_error(r_exp(.seed = "error"))
})

test_that("r_exp() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_exp(.seed = 100)), 100)
  expect_null(pull_seed(r_exp()))
  expect_false(is.null(pull_seed(r_exp(.seed = T))))
})

test_that("r_exp() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_exp(.seed = 123456),
    {
      set.seed(123456)
      stats::rexp(100)
    },
    check.attributes = F
  )

  expect_equal(
    r_exp(1 / 10, .seed = 9876),
    {
      set.seed(9876)
      stats::rexp(100, 1 / 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_exp(1 / c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rexp(4, 1 / c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_fdist() ####

test_that("r_fdist() lengths work", {

  # Check we're getting the default length
  expect_length(r_fdist(10, 10), 100)

  # Check we can overwrite the length manually
  expect_length(r_fdist(10, 10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_fdist(df1 = c(5, 10), 10), 2)
  expect_length(r_fdist(10, df2 = c(5, 10)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_fdist(10, 10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_fdist(10, 10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_fdist(10, 10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_fdist(10, 10))$x)), 1)
})

test_that("r_fdist() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_fdist(df1 = -10, 10))
  expect_error(r_fdist(df1 = 0, 10))

  expect_error(r_fdist(10, df2 = -10))
  expect_error(r_fdist(10, df2 = 0))

  expect_error(r_fdist(df1 = c(1, 2, 3), df2 = c(2, 1)))

  expect_error(r_fdist(n = -1))
  expect_error(r_fdist(n = c(1, 2)))
  expect_error(r_fdist(n = 1.5))

  expect_error(r_fdist(10, 10, .seed = -2^31))
  expect_error(r_fdist(10, 10, .seed = 2^31))
  expect_error(r_fdist(10, 10, .seed = FALSE))
  expect_error(r_fdist(10, 10, .seed = "error"))
})

test_that("r_fdist() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_fdist(10, 10, .seed = 100)), 100)
  expect_null(pull_seed(r_fdist(10, 10)))
  expect_false(is.null(pull_seed(r_fdist(10, 10, .seed = T))))
})

test_that("r_fdist() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_fdist(10, 10, .seed = 123456),
    {
      set.seed(123456)
      stats::rf(100, 10, 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_fdist(10, 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rf(100, 10, 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_fdist(c(10, 23, 100, 91), 4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rf(4, c(10, 23, 100, 91), 4)
    },
    check.attributes = F
  )
})

#### r_gamma() ####

test_that("r_gamma() lengths work", {

  # Check we're getting the default length
  expect_length(r_gamma(1), 100)

  # Check we can overwrite the length manually
  expect_length(r_gamma(1, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_gamma(shape = c(1, 2)), 2)
  expect_length(r_gamma(1, scale = c(1, 2)), 2)
  expect_length(r_gamma(1, rate = c(1, 2)), 2)
  expect_length(r_gamma(1, mean = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_gamma(1))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_gamma(1))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_gamma(1))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_gamma(1))$x)), 1)
})

test_that("r_gamma() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_gamma(shape = -1))
  expect_error(r_gamma(shape = 0))

  expect_error(r_gamma(1, scale = -1))
  expect_error(r_gamma(1, scale = 0))

  expect_error(r_gamma(1, rate = -1))
  expect_error(r_gamma(1, rate = 0))

  expect_error(r_gamma(1, mean = -1))
  expect_error(r_gamma(1, mean = 0))

  expect_error(r_gamma(1, scale = 1, rate = 1))
  expect_error(r_gamma(1, scale = 1, mean = 1))
  expect_error(r_gamma(1, rate = 1, mean = 1))
  expect_error(r_gamma(1, scale = 1, rate = 1, mean = 1))

  expect_error(r_gamma(shape = c(1, 2, 3), scale = c(0.5, 1)))
  expect_error(r_gamma(shape = c(1, 2, 3), rate = c(0.5, 1)))
  expect_error(r_gamma(shape = c(1, 2, 3), mean = c(0.5, 1)))

  expect_error(r_gamma(1, n = -1))
  expect_error(r_gamma(1, n = c(1, 2)))
  expect_error(r_gamma(1, n = 1.5))

  expect_error(r_gamma(1, .seed = -2^31))
  expect_error(r_gamma(1, .seed = 2^31))
  expect_error(r_gamma(1, .seed = FALSE))
  expect_error(r_gamma(1, .seed = "error"))
})

test_that("r_gamma() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_gamma(1, .seed = 100)), 100)
  expect_null(pull_seed(r_gamma(1)))
  expect_false(is.null(pull_seed(r_gamma(1, .seed = T))))
})

test_that("r_gamma() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_gamma(1, .seed = 123456),
    {
      set.seed(123456)
      stats::rgamma(100, 1)
    },
    check.attributes = F
  )

  expect_equal(
    r_gamma(10, scale = 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rgamma(100, 10, scale = 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_gamma(10, rate = 2, .seed = 523214),
    {
      set.seed(523214)
      stats::rgamma(100, 10, rate = 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_gamma(10, mean = 2, .seed = 88173),
    {
      set.seed(88173)
      stats::rgamma(100, 10, scale = 1 / 5)
    },
    check.attributes = F
  )

  expect_equal(
    r_gamma(c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rgamma(4, c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_geom() ####

test_that("r_geom() lengths work", {

  # Check we're getting the default length
  expect_length(r_geom(), 100)

  # Check we can overwrite the length manually
  expect_length(r_geom(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_geom(prob = c(0.2, 0.4)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_geom())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_geom())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_geom())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_geom())$x)), 1)
})

test_that("r_geom() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_geom(prob = -1))
  expect_error(r_geom(prob = 0))
  expect_error(r_geom(prob = 1.5))

  expect_error(r_geom(n = -1))
  expect_error(r_geom(n = c(1, 2)))
  expect_error(r_geom(n = 1.5))

  expect_error(r_geom(.seed = -2^31))
  expect_error(r_geom(.seed = 2^31))
  expect_error(r_geom(.seed = FALSE))
  expect_error(r_geom(.seed = "error"))
})

test_that("r_geom() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_geom(.seed = 100)), 100)
  expect_null(pull_seed(r_geom()))
  expect_false(is.null(pull_seed(r_geom(.seed = T))))
})

test_that("r_geom() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_geom(.seed = 123456),
    {
      set.seed(123456)
      stats::rgeom(100, 0.5)
    },
    check.attributes = F
  )

  expect_equal(
    r_geom(0.2, .seed = 9876),
    {
      set.seed(9876)
      stats::rgeom(100, 0.2)
    },
    check.attributes = F
  )

  expect_equal(
    r_geom(1 / c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rgeom(4, 1 / c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_hyper() ####

test_that("r_hyper() lengths work", {

  # Check we're getting the default length
  expect_length(r_hyper(10, 5, 5), 100)

  # Check we can overwrite the length manually
  expect_length(r_hyper(10, 5, 5, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_hyper(total = c(8, 10), 5, 5), 2)
  expect_length(r_hyper(10, positives = c(5, 6), 5), 2)
  expect_length(r_hyper(10, 5, num = c(5, 6)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_hyper(10, 5, 5))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_hyper(10, 5, 5))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_hyper(10, 5, 5))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_hyper(10, 5, 5))$x)), 1)
})

test_that("r_hyper() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_hyper(total = 10.5, 5, 5))
  expect_error(r_hyper(total = -10, 5, 5))

  expect_error(r_hyper(10, positive = 5.5, 5, 5))
  expect_error(r_hyper(10, positive = 15, 5, 5))

  expect_error(r_hyper(10, 5, num = 5.5))
  expect_error(r_hyper(10, 5, num = 15))

  expect_error(r_hyper(total = c(10, 15, 20), positive = c(5, 6), 5))
  expect_error(r_hyper(total = c(10, 15, 20), 5, num = c(5, 6)))
  expect_error(r_hyper(10, positive = c(5, 6, 7), num = c(5, 6)))

  expect_error(r_hyper(10, 5, 5, n = -1))
  expect_error(r_hyper(10, 5, 5, n = c(1, 2)))
  expect_error(r_hyper(10, 5, 5, n = 1.5))

  expect_error(r_hyper(10, 5, 5, .seed = -2^31))
  expect_error(r_hyper(10, 5, 5, .seed = 2^31))
  expect_error(r_hyper(10, 5, 5, .seed = FALSE))
  expect_error(r_hyper(10, 5, 5, .seed = "error"))
})

test_that("r_hyper() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_hyper(10, 5, 5, .seed = 100)), 100)
  expect_null(pull_seed(r_hyper(10, 5, 5)))
  expect_false(is.null(pull_seed(r_hyper(10, 5, 5, .seed = T))))
})

test_that("r_hyper() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_hyper(10, 5, 5, .seed = 123456),
    {
      set.seed(123456)
      stats::rhyper(100, 5, 5, 5)
    },
    check.attributes = F
  )

  expect_equal(
    r_hyper(10, 2, 8, .seed = 9876),
    {
      set.seed(9876)
      stats::rhyper(100, 2, 8, 8)
    },
    check.attributes = F
  )

  expect_equal(
    r_hyper(c(10, 23, 100, 91), 2, 8, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rhyper(4, 2, c(10, 23, 100, 91) - 2, 8)
    },
    check.attributes = F
  )
})

#### r_lnorm()) ####

test_that("r_lnorm() lengths work", {

  # Check we're getting the default length
  expect_length(r_lnorm(), 100)

  # Check we can overwrite the length manually
  expect_length(r_lnorm(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_lnorm(mean_log = c(-10, 10)), 2)
  expect_length(r_lnorm(sd_log = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_lnorm())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_lnorm())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_lnorm())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_lnorm())$x)), 1)
})

test_that("r_lnorm() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_lnorm(sd_log = -1))
  expect_error(r_lnorm(sd_log = 0))

  expect_error(r_lnorm(mean_log = c(1, 2, 3), sd_log = c(0.5, 1)))

  expect_error(r_lnorm(n = -1))
  expect_error(r_lnorm(n = c(1, 2)))
  expect_error(r_lnorm(n = 1.5))

  expect_error(r_lnorm(.seed = -2^31))
  expect_error(r_lnorm(.seed = 2^31))
  expect_error(r_lnorm(.seed = FALSE))
  expect_error(r_lnorm(.seed = "error"))
})

test_that("r_lnorm() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_lnorm(.seed = 100)), 100)
  expect_null(pull_seed(r_lnorm()))
  expect_false(is.null(pull_seed(r_lnorm(.seed = T))))
})

test_that("r_lnorm() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_lnorm(.seed = 123456),
    {
      set.seed(123456)
      stats::rlnorm(100)
    },
    check.attributes = F
  )

  expect_equal(
    r_lnorm(10, 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rlnorm(100, 10, 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_lnorm(c(10, 23, 100, 91), 4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rlnorm(4, c(10, 23, 100, 91), 4)
    },
    check.attributes = F
  )
})

#### r_nbinom() ####

test_that("r_nbinom() lengths work", {

  # Check we're getting the default length
  expect_length(r_nbinom(10), 100)

  # Check we can overwrite the length manually
  expect_length(r_nbinom(10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_nbinom(r = c(10, 20)), 2)
  expect_length(r_nbinom(10, prob = c(0.2, 0.4)), 2)
  expect_length(r_nbinom(10, mu = c(10, 20)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_nbinom(10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_nbinom(10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_nbinom(10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_nbinom(10))$x)), 1)
})

test_that("r_nbinom() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_nbinom(r = -1))
  expect_error(r_nbinom(r = 0))

  expect_error(r_nbinom(10, prob = -1))
  expect_error(r_nbinom(10, prob = 1.5))

  expect_error(r_nbinom(10, mu = -10))
  expect_error(r_nbinom(10, mu = 0))

  expect_error(r_nbinom(prob = 0.2))
  expect_error(r_nbinom(r = 10, prob = 0.2, mu = 20))

  expect_error(r_nbinom(r = c(10, 20, 30), prob = c(0.5, 0.7)))
  expect_error(r_nbinom(r = c(10, 20, 30), mu = c(20, 10)))
  expect_error(r_nbinom(prob = c(0.1, 0.2, 0.3), mu = c(20, 10)))

  expect_error(r_nbinom(10, n = -1))
  expect_error(r_nbinom(10, n = c(1, 2)))
  expect_error(r_nbinom(10, n = 1.5))

  expect_error(r_nbinom(10, .seed = -2^31))
  expect_error(r_nbinom(10, .seed = 2^31))
  expect_error(r_nbinom(10, .seed = FALSE))
  expect_error(r_nbinom(10, .seed = "error"))
})

test_that("r_nbinom() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_nbinom(10, .seed = 100)), 100)
  expect_null(pull_seed(r_nbinom(10)))
  expect_false(is.null(pull_seed(r_nbinom(10, .seed = T))))
})

test_that("r_nbinom() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_nbinom(10, .seed = 123456),
    {
      set.seed(123456)
      stats::rnbinom(100, 10, 0.5)
    },
    check.attributes = F
  )

  expect_equal(
    r_nbinom(prob = 0.2, mu = 20, .seed = 9876),
    {
      set.seed(9876)
      stats::rnbinom(100, 80, 0.8)
    },
    check.attributes = F
  )

  expect_equal(
    r_nbinom(10, mu = 30, .seed = 9876),
    {
      set.seed(9876)
      stats::rnbinom(100, 10, mu = 30)
    },
    check.attributes = F
  )

  expect_equal(
    r_nbinom(c(10, 23, 100, 91), 0.4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rnbinom(4, c(10, 23, 100, 91), 0.6)
    },
    check.attributes = F
  )
})

#### r_pois() ####

test_that("r_pois() lengths work", {

  # Check we're getting the default length
  expect_length(r_pois(1), 100)

  # Check we can overwrite the length manually
  expect_length(r_pois(1, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_pois(rate = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_pois(1))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_pois(1))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_pois(1))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_pois(1))$x)), 1)
})

test_that("r_pois() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_pois(rate = -1))
  expect_error(r_pois(rate = 0))

  expect_error(r_pois(1, n = -1))
  expect_error(r_pois(1, n = c(1, 2)))
  expect_error(r_pois(1, n = 1.5))

  expect_error(r_pois(1, .seed = -2^31))
  expect_error(r_pois(1, .seed = 2^31))
  expect_error(r_pois(1, .seed = FALSE))
  expect_error(r_pois(1, .seed = "error"))
})

test_that("r_pois() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_pois(1, .seed = 100)), 100)
  expect_null(pull_seed(r_pois(1)))
  expect_false(is.null(pull_seed(r_pois(1, .seed = T))))
})

test_that("r_pois() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_pois(1, .seed = 123456),
    {
      set.seed(123456)
      stats::rpois(100, 1)
    },
    check.attributes = F
  )

  expect_equal(
    r_pois(10, .seed = 9876),
    {
      set.seed(9876)
      stats::rpois(100, 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_pois(c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rpois(4, c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_tdist() ####

test_that("r_tdist() lengths work", {

  # Check we're getting the default length
  expect_length(r_tdist(10), 100)

  # Check we can overwrite the length manually
  expect_length(r_tdist(10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_tdist(df = c(10, 20)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_tdist(10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_tdist(10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_tdist(10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_tdist(10))$x)), 1)
})

test_that("r_tdist() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_tdist(df = -1))
  expect_error(r_tdist(df = 0))

  expect_error(r_tdist(10, n = -1))
  expect_error(r_tdist(10, n = c(1, 2)))
  expect_error(r_tdist(10, n = 1.5))

  expect_error(r_tdist(10, .seed = -2^31))
  expect_error(r_tdist(10, .seed = 2^31))
  expect_error(r_tdist(10, .seed = FALSE))
  expect_error(r_tdist(10, .seed = "error"))
})

test_that("r_tdist() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_tdist(10, .seed = 100)), 100)
  expect_null(pull_seed(r_tdist(10)))
  expect_false(is.null(pull_seed(r_tdist(10, .seed = T))))
})

test_that("r_tdist() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_tdist(10, .seed = 123456),
    {
      set.seed(123456)
      stats::rt(100, 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_tdist(30, .seed = 9876),
    {
      set.seed(9876)
      stats::rt(100, 30)
    },
    check.attributes = F
  )

  expect_equal(
    r_tdist(c(10, 23, 100, 91), .seed = 7214361),
    {
      set.seed(7214361)
      stats::rt(4, c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_unif() ####

test_that("r_unif() lengths work", {

  # Check we're getting the default length
  expect_length(r_unif(), 100)

  # Check we can overwrite the length manually
  expect_length(r_unif(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_unif(min = c(-1, -2)), 2)
  expect_length(r_unif(max = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_unif())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_unif())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_unif())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_unif())$x)), 1)
})

test_that("r_unif() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_unif(min = 2))
  expect_error(r_unif(max = -1))
  expect_error(r_unif(min = c(10, 20), max = 15))
  expect_error(r_unif(min = 15, max = c(10, 20)))

  expect_error(r_unif(min = c(1, 2, 3), max = c(5, 6)))

  expect_error(r_unif(n = -1))
  expect_error(r_unif(n = c(1, 2)))
  expect_error(r_unif(n = 1.5))

  expect_error(r_unif(.seed = -2^31))
  expect_error(r_unif(.seed = 2^31))
  expect_error(r_unif(.seed = FALSE))
  expect_error(r_unif(.seed = "error"))
})

test_that("r_unif() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_unif(.seed = 100)), 100)
  expect_null(pull_seed(r_unif()))
  expect_false(is.null(pull_seed(r_unif(.seed = T))))
})

test_that("r_unif() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_unif(.seed = 123456),
    {
      set.seed(123456)
      stats::runif(100)
    },
    check.attributes = F
  )

  expect_equal(
    r_unif(5, 100, .seed = 9876),
    {
      set.seed(9876)
      stats::runif(100, 5, 100)
    },
    check.attributes = F
  )

  expect_equal(
    r_unif(c(10, 23, 100, 91), 100, .seed = 7214361),
    {
      set.seed(7214361)
      stats::runif(4, c(10, 23, 100, 91), 100)
    },
    check.attributes = F
  )
})

#### r_weibull() ####

test_that("r_weibull() lengths work", {

  # Check we're getting the default length
  expect_length(r_weibull(10), 100)

  # Check we can overwrite the length manually
  expect_length(r_weibull(10, n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_weibull(shape = c(5, 10)), 2)
  expect_length(r_weibull(10, scale = c(1, 2)), 2)
  expect_length(r_weibull(10, b_scale = c(1, 2)), 2)
  expect_length(r_weibull(10, B_scale = c(1, 2)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_weibull(10))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_weibull(10))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_weibull(10))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_weibull(10))$x)), 1)
})

test_that("r_weibull() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_weibull(shape = -1))
  expect_error(r_weibull(shape = 0))

  expect_error(r_weibull(10, scale = -1))
  expect_error(r_weibull(10, scale = 0))

  expect_error(r_weibull(10, b_scale = -1))
  expect_error(r_weibull(10, b_scale = 0))

  expect_error(r_weibull(10, B_scale = -1))
  expect_error(r_weibull(10, B_scale = 0))

  expect_error(r_weibull(10, scale = 1, b_scale = 1))
  expect_error(r_weibull(10, scale = 1, B_scale = 1))
  expect_error(r_weibull(10, b_scale = 1, b_scale = 1))

  expect_error(r_weibull(shape = c(10, 20, 30), scale = c(1, 2)))
  expect_error(r_weibull(shape = c(10, 20, 30), b_scale = c(1, 2)))
  expect_error(r_weibull(shape = c(10, 20, 30), B_scale = c(1, 2)))

  expect_error(r_weibull(10, n = -1))
  expect_error(r_weibull(10, n = c(1, 2)))
  expect_error(r_weibull(10, n = 1.5))

  expect_error(r_weibull(10, .seed = -2^31))
  expect_error(r_weibull(10, .seed = 2^31))
  expect_error(r_weibull(10, .seed = FALSE))
  expect_error(r_weibull(10, .seed = "error"))
})

test_that("r_weibull() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_weibull(10, .seed = 100)), 100)
  expect_null(pull_seed(r_weibull(10)))
  expect_false(is.null(pull_seed(r_weibull(10, .seed = T))))
})

test_that("r_weibull() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_weibull(10, .seed = 123456),
    {
      set.seed(123456)
      stats::rweibull(100, 10)
    },
    check.attributes = F
  )

  expect_equal(
    r_weibull(10, scale = 2, .seed = 9876),
    {
      set.seed(9876)
      stats::rweibull(100, 10, 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_weibull(10, b_scale = 2, .seed = 11234),
    {
      set.seed(11234)
      stats::rweibull(100, 10, 2^10)
    },
    check.attributes = F
  )

  expect_equal(
    r_weibull(10, B_scale = 2, .seed = 2245212),
    {
      set.seed(2245212)
      stats::rweibull(100, 10, 1 / 2)
    },
    check.attributes = F
  )

  expect_equal(
    r_weibull(c(10, 23, 100, 91), 4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rweibull(4, c(10, 23, 100, 91), 4)
    },
    check.attributes = F
  )
})

#### r_sample() ####

test_that("r_sample() lengths work", {

  # Check we're getting the default length
  expect_length(r_sample(c("A", "B")), 100)

  # Check we can overwrite the length manually
  expect_length(r_sample(c("A", "B"), n = 50), 50)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_sample(c("A", "B")))$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_sample(c("A", "B")))$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_sample(c("A", "B")))$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_sample(c("A", "B")))$x)), 1)
})

test_that("r_sample() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_sample(c("A", "B"), weights = c(-1, 2)))

  expect_error(r_sample(sample = c("A", "B"), weights = c(1, 2, 3)))

  expect_error(r_sample(c("A", "B"), n = -1))
  expect_error(r_sample(c("A", "B"), n = c(1, 2)))
  expect_error(r_sample(c("A", "B"), n = 1.5))

  expect_error(r_sample(c("A", "B"), .seed = -2^31))
  expect_error(r_sample(c("A", "B"), .seed = 2^31))
  expect_error(r_sample(c("A", "B"), .seed = FALSE))
  expect_error(r_sample(c("A", "B"), .seed = "error"))
})

test_that("r_sample() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_sample(c("A", "B"), .seed = 100)), 100)
  expect_null(pull_seed(r_sample(c("A", "B"))))
  expect_false(is.null(pull_seed(r_sample(c("A", "B"), .seed = T))))
})

test_that("r_sample() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_sample(c("A", "B"), .seed = 123456),
    {
      set.seed(123456)
      base::sample(c("A", "B"), 100, replace = T, prob = c(0.5, 0.5))
    },
    check.attributes = F
  )

  expect_equal(
    r_sample(c("A", "B"), c(10, 30), .seed = 9876),
    {
      set.seed(9876)
      base::sample(c("A", "B"), 100, replace = T, prob = c(0.25, 0.75))
    },
    check.attributes = F
  )
})
