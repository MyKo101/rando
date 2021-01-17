
set_n(100)
df <- tibble(id = 1:25)

#### r_bern() ####

test_that("r_bern() lengths work", {

  # Check we're getting the default length
  expect_length(r_bern(), 100)

  # Check we can overwrite the length manually
  expect_length(r_bern(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_bern(prob = c(0.2, 0.5)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_bern())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_bern())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_bern())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_bern())$x)), 1)
})

test_that("r_bern() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_bern(prob = -1))
  expect_error(r_bern(prob = 1.5))

  expect_error(r_bern(n = -1))
  expect_error(r_bern(n = c(1, 2)))
  expect_error(r_bern(n = 1.5))

  expect_error(r_bern(.seed = -2^31))
  expect_error(r_bern(.seed = 2^31))
  expect_error(r_bern(.seed = FALSE))
  expect_error(r_bern(.seed = "error"))
})

test_that("r_bern() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_bern(.seed = 100)), 100)
  expect_null(pull_seed(r_bern()))
  expect_false(is.null(pull_seed(r_bern(.seed = T))))
})

test_that("r_bern() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_bern(.seed = 123456),
    {
      set.seed(123456)
      stats::rbinom(100, 1, 0.5)
    },
    check.attributes = F
  )

  expect_equal(
    r_bern(0.2, .seed = 9876),
    {
      set.seed(9876)
      stats::rbinom(100, 1, 0.2)
    },
    check.attributes = F
  )

  expect_equal(
    r_bern(1 / c(10, 23, 100, 91), 0.4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rbinom(4, 1, 1 / c(10, 23, 100, 91))
    },
    check.attributes = F
  )
})

#### r_lgl() ####

test_that("r_lgl() lengths work", {

  # Check we're getting the default length
  expect_length(r_lgl(), 100)

  # Check we can overwrite the length manually
  expect_length(r_lgl(n = 50), 50)

  # Check the length can be extracted from arguments
  expect_length(r_lgl(prob = c(0.2, 0.5)), 2)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_lgl())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_lgl())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_lgl())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_lgl())$x)), 1)
})

test_that("r_lgl() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_lgl(prob = -1))
  expect_error(r_lgl(prob = 1.5))

  expect_error(r_lgl(n = -1))
  expect_error(r_lgl(n = c(1, 2)))
  expect_error(r_lgl(n = 1.5))

  expect_error(r_lgl(.seed = -2^31))
  expect_error(r_lgl(.seed = 2^31))
  expect_error(r_lgl(.seed = FALSE))
  expect_error(r_lgl(.seed = "error"))
})

test_that("r_lgl() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_lgl(.seed = 100)), 100)
  expect_null(pull_seed(r_lgl()))
  expect_false(is.null(pull_seed(r_lgl(.seed = T))))
})

test_that("r_lgl() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_lgl(.seed = 123456),
    {
      set.seed(123456)
      stats::rbinom(100, 1, 0.5) == 1
    },
    check.attributes = F
  )

  expect_equal(
    r_lgl(0.2, .seed = 9876),
    {
      set.seed(9876)
      stats::rbinom(100, 1, 0.2) == 1
    },
    check.attributes = F
  )

  expect_equal(
    r_lgl(1 / c(10, 23, 100, 91), 0.4, .seed = 7214361),
    {
      set.seed(7214361)
      stats::rbinom(4, 1, 1 / c(10, 23, 100, 91)) == 1
    },
    check.attributes = F
  )
})

#### r_letters() ####

test_that("r_letters() lengths work", {

  # Check we're getting the default length
  expect_length(r_letters(), 100)

  # Check we can overwrite the length manually
  expect_length(r_letters(n = 50), 50)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_letters())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_letters())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_letters())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_letters())$x)), 1)
})

test_that("r_letters() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_letters(nchar = -1))
  expect_error(r_letters(nchar = 1.5))

  expect_error(r_letters(n = -1))
  expect_error(r_letters(n = c(1, 2)))
  expect_error(r_letters(n = 1.5))

  expect_error(r_letters(.seed = -2^31))
  expect_error(r_letters(.seed = 2^31))
  expect_error(r_letters(.seed = FALSE))
  expect_error(r_letters(.seed = "error"))
})

test_that("r_letters() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_letters(.seed = 100)), 100)
  expect_null(pull_seed(r_letters()))
  expect_false(is.null(pull_seed(r_letters(.seed = T))))
})

test_that("r_letters() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_letters(.seed = 123456),
    {
      set.seed(123456)
      base::sample(letters, 100, replace = T)
    },
    check.attributes = F
  )

  expect_equal(
    r_letters(nchar = 3, n = 4, .seed = 143141),
    {
      set.seed(143141)
      res <- base::sample(letters, 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:6], collapse = ""),
        paste0(res[7:9], collapse = ""),
        paste0(res[10:12], collapse = "")
      )
    },
    check.attributes = F
  )

  expect_equal(
    r_letters(nchar = c(3, 4, 5), .seed = 9134113),
    {
      set.seed(9134113)
      res <- base::sample(letters, 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:7], collapse = ""),
        paste0(res[8:12], collapse = "")
      )
    },
    check.attributes = F
  )
})

#### r_LETTERS() ####

test_that("r_LETTERS() lengths work", {

  # Check we're getting the default length
  expect_length(r_LETTERS(), 100)

  # Check we can overwrite the length manually
  expect_length(r_LETTERS(n = 50), 50)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_LETTERS())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_LETTERS())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_LETTERS())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_LETTERS())$x)), 1)
})

test_that("r_LETTERS() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_LETTERS(nchar = -1))
  expect_error(r_LETTERS(nchar = 1.5))

  expect_error(r_LETTERS(n = -1))
  expect_error(r_LETTERS(n = c(1, 2)))
  expect_error(r_LETTERS(n = 1.5))

  expect_error(r_LETTERS(.seed = -2^31))
  expect_error(r_LETTERS(.seed = 2^31))
  expect_error(r_LETTERS(.seed = FALSE))
  expect_error(r_LETTERS(.seed = "error"))
})

test_that("r_LETTERS() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_LETTERS(.seed = 100)), 100)
  expect_null(pull_seed(r_LETTERS()))
  expect_false(is.null(pull_seed(r_LETTERS(.seed = T))))
})

test_that("r_LETTERS() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_LETTERS(.seed = 123456),
    {
      set.seed(123456)
      base::sample(LETTERS, 100, replace = T)
    },
    check.attributes = F
  )

  expect_equal(
    r_LETTERS(nchar = 3, n = 4, .seed = 143141),
    {
      set.seed(143141)
      res <- base::sample(LETTERS, 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:6], collapse = ""),
        paste0(res[7:9], collapse = ""),
        paste0(res[10:12], collapse = "")
      )
    },
    check.attributes = F
  )

  expect_equal(
    r_LETTERS(nchar = c(3, 4, 5), .seed = 9134113),
    {
      set.seed(9134113)
      res <- base::sample(LETTERS, 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:7], collapse = ""),
        paste0(res[8:12], collapse = "")
      )
    },
    check.attributes = F
  )
})

#### r_Letters() ####

test_that("r_Letters() lengths work", {

  # Check we're getting the default length
  expect_length(r_Letters(), 100)

  # Check we can overwrite the length manually
  expect_length(r_Letters(n = 50), 50)

  # Check lengths can be found in tibble()
  expect_length(tibble(id = 1:10, x = r_Letters())$x, 10)
  expect_gt(length(unique(tibble(id = 1:10, x = r_Letters())$x)), 1)

  # Check lengths can be found in dplyr::mutate()
  expect_length(dplyr::mutate(df, x = r_Letters())$x, 25)
  expect_gt(length(unique(dplyr::mutate(df, x = r_Letters())$x)), 1)
})

test_that("r_Letters() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_Letters(nchar = -1))
  expect_error(r_Letters(nchar = 1.5))

  expect_error(r_Letters(n = -1))
  expect_error(r_Letters(n = c(1, 2)))
  expect_error(r_Letters(n = 1.5))

  expect_error(r_Letters(.seed = -2^31))
  expect_error(r_Letters(.seed = 2^31))
  expect_error(r_Letters(.seed = FALSE))
  expect_error(r_Letters(.seed = "error"))
})

test_that("r_Letters() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_Letters(.seed = 100)), 100)
  expect_null(pull_seed(r_Letters()))
  expect_false(is.null(pull_seed(r_Letters(.seed = T))))
})

test_that("r_Letters() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_Letters(.seed = 123456),
    {
      set.seed(123456)
      base::sample(c(letters, LETTERS), 100, replace = T)
    },
    check.attributes = F
  )

  expect_equal(
    r_Letters(nchar = 3, n = 4, .seed = 143141),
    {
      set.seed(143141)
      res <- base::sample(c(letters, LETTERS), 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:6], collapse = ""),
        paste0(res[7:9], collapse = ""),
        paste0(res[10:12], collapse = "")
      )
    },
    check.attributes = F
  )

  expect_equal(
    r_Letters(nchar = c(3, 4, 5), .seed = 9134113),
    {
      set.seed(9134113)
      res <- base::sample(c(letters, LETTERS), 12, replace = T)
      c(
        paste0(res[1:3], collapse = ""),
        paste0(res[4:7], collapse = ""),
        paste0(res[8:12], collapse = "")
      )
    },
    check.attributes = F
  )
})

#### r_matrix() ####

test_that("r_matrix() lengths work", {

  # Check we're getting the default length
  expect_equal(nrow(r_matrix(r_norm)), 100)
  expect_equal(ncol(r_matrix(r_norm)), 100)

  # Check we can overwrite the length manually
  expect_equal(nrow(r_matrix(r_norm,nrow=50)), 50)
  expect_equal(ncol(r_matrix(r_norm,nrow=50)), 100)
  expect_equal(nrow(r_matrix(r_norm,ncol=50)), 100)
  expect_equal(ncol(r_matrix(r_norm,ncol=50)), 50)

  # Check the length can be extracted from arguments
  expect_equal(nrow(r_matrix(r_norm,row_names = c("One","Two","Three"))),3)
  expect_equal(ncol(r_matrix(r_norm,col_names = c("One","Two","Three"))),3)

  # Check lengths can be found in tibble()
  expect_equal(nrow(tibble(id=1:10,x=r_matrix(r_norm))$x),10)
  expect_equal(ncol(tibble(id=1:10,x=r_matrix(r_norm))$x),10)

  expect_gt(nrow(unique(tibble(id=1:10,x=r_matrix(r_norm))$x)),1)
  expect_gt(ncol(unique(tibble(id=1:10,x=r_matrix(r_norm))$x)),1)


  # Check lengths can be found in dplyr::mutate()
  expect_equal(nrow(dplyr::mutate(df,x=r_matrix(r_norm))$x),25)
  expect_equal(ncol(dplyr::mutate(df,x=r_matrix(r_norm))$x),25)

  expect_gt(nrow(unique(dplyr::mutate(df,x=r_matrix(r_norm))$x)),1)
  expect_gt(ncol(unique(dplyr::mutate(df,x=r_matrix(r_norm))$x)),1)

})

test_that("r_matrix() errors work", {

  # Check invalid parameters throw errors
  expect_error(r_matrix(r_norm,nrow = -1))
  expect_error(r_matrix(r_norm,nrow = 1.5))
  expect_error(r_matrix(r_norm,nrow = c(1,2)))

  expect_error(r_matrix(r_norm,ncol = -1))
  expect_error(r_matrix(r_norm,ncol = 1.5))
  expect_error(r_matrix(r_norm,ncol = c(1,2)))


  expect_error(r_matrix(r_norm,.seed = -2^31))
  expect_error(r_matrix(r_norm,.seed = 2^31))
  expect_error(r_matrix(r_norm,.seed = FALSE))
  expect_error(r_matrix(r_norm,.seed = "error"))
})

test_that("r_matrix() seeds work", {

  # Check we can extract the seed
  expect_equal(pull_seed(r_matrix(r_norm,.seed = 100)), 100)
  expect_null(pull_seed(r_matrix(r_norm)))
  expect_false(is.null(pull_seed(r_matrix(r_norm,.seed = T))))
})

test_that("r_matrix() results are consistent", {

  # Check we are getting the same result when the seed is set
  expect_equal(
    r_matrix(r_norm,.seed = 123456),
    {
      set.seed(123456)
      matrix(rnorm(100*100),nrow=100,ncol=100)
    },
    check.attributes = F
  )

  expect_equal(
    r_matrix(r_norm,mean=100,sd=10,.seed = 8963),
    {
      set.seed(8963)
      matrix(rnorm(100*100,mean=100,sd=10),nrow=100,ncol=100)
    },
    check.attributes = F
  )


  expect_equal(
    r_matrix(r_unif,min=100,max=200,.seed = 66534),
    {
      set.seed(66534)
      matrix(runif(100*100,min=100,max=200),nrow=100,ncol=100)
    },
    check.attributes = F
  )

})
