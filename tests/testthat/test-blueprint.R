set_n(100)
df <- tibble(id = 1:25)

bp <- blueprint(
  x = r_norm(),
  y = r_unif(),
  z = r_letters()
)

test_that("Blueprints are produced", {
  expect_type(
    blueprint(x = r_norm(), y = rnorm()),
    "closure"
  )

  expect_s3_class(
    blueprint(x = r_norm(), y = rnorm()),
    "function"
  )

  expect_s3_class(
    blueprint(x = r_norm(), y = rnorm()),
    "rando_blueprint_function"
  )

  bp_like_env <- rlang::new_environment(
    list(set_blueprint_n = rando:::set_blueprint_n),
    .GlobalEnv
  )

  bp_like_args <- list(
    `...`=rlang::missing_arg(),
    n = quote(default_n(...)),
    .seed = NULL)



  expect_equal(
    blueprint(x = r_norm(), y = rnorm()),
    structure(
      rlang::new_function(
        bp_like_args,
        quote({
          set_blueprint_n(n)
          on.exit(set_blueprint_n())
          list2env(list(...), environment())
          with_seed(
            .seed,
            tibble::tibble(
              x = r_norm(),
              y = rnorm(),
              .rows = n
            )
          )
        }),
        bp_like_env
      ),
      class = c("rando_blueprint_function","function")
    )
  )



  expect_equal(
    blueprint(x = r_unif(), y = r_unif()),
    structure(
      rlang::new_function(
        bp_like_args,
        quote({
          set_blueprint_n(n)
          on.exit(set_blueprint_n())
          list2env(list(...), environment())
          with_seed(
            .seed,
            tibble::tibble(
              x = r_unif(),
              y = r_unif(),
              .rows = n
            )
          )
        }),
        bp_like_env
      ),
      class = c("rando_blueprint_function","function")
    )
  )




  expect_true(
    is_blueprint(bp)
  )
  expect_false(
    !is_blueprint(bp)
  )
  expect_false(
    is_blueprint(bp(n = 10))
  )
})

test_that("Blueprints work", {
  expect_s3_class(
    bp(n = 100),
    "tbl_df"
  )

  expect_s3_class(
    bp(n = 100),
    "tbl"
  )

  expect_s3_class(
    bp(n = 100),
    "data.frame"
  )

  expect_equal(
    pull_seed(bp(n = 30, .seed = 1263)),
    1263
  )

  expect_equal(
    bp(n = 100, .seed = 12719),
    {
      set.seed(12719)
      tibble(
        x = rnorm(100),
        y = runif(100),
        z = sample(letters, 100, replace = T)
      )
    },
    check.attributes = F
  )

  expect_equal(
    bp(n = 10000, .seed = 112314),
    {
      set.seed(112314)
      tibble(
        x = rnorm(10000),
        y = runif(10000),
        z = sample(letters, 10000, replace = T)
      )
    },
    check.attributes = F
  )
})

test_that("bp_where works", {
  expect_s3_class(
    bp_where(rep(T, 5), bp),
    "tbl_df"
  )
  expect_s3_class(
    bp_where(rep(T, 5), bp),
    "tbl"
  )
  expect_s3_class(
    bp_where(rep(T, 5), bp),
    "data.frame"
  )

  expect_equal(
    bp_where(rep(T, 5), bp, .seed = 471409),
    {
      set.seed(471409)
      tibble(
        x = rnorm(n = 5),
        y = runif(n = 5),
        z = sample(letters, 5, replace = T)
      )
    },
    check.attributes = F
  )

  expect_equal(
    pull_seed(bp_where(rep(T, 5), bp, .seed = 1594)),
    1594
  )

  expect_equal(
    dplyr::mutate(df,
      cnd = r_lgl(.seed = 32),
      bp_where(cnd, bp, .seed = 11492)
    ),
    {
      df2 <- dplyr::mutate(df, cnd = r_lgl(.seed = 32), x = NA, y = NA, z = NA)
      bp_tbl <- bp(n = sum(df2$cnd), .seed = 11492)
      df2$x[df2$cnd] <- bp_tbl$x
      df2$y[df2$cnd] <- bp_tbl$y
      df2$z[df2$cnd] <- bp_tbl$z
      df2
    }
  )


  expect_equal(
    dplyr::mutate(df,
      cnd = r_lgl(.seed = 1818722),
      bp_where(cnd, bp, .seed = 23214)
    ),
    {
      df2 <- dplyr::mutate(df, cnd = r_lgl(.seed = 1818722), x = NA, y = NA, z = NA)
      bp_tbl <- bp(n = sum(df2$cnd), .seed = 23214)
      df2$x[df2$cnd] <- bp_tbl$x
      df2$y[df2$cnd] <- bp_tbl$y
      df2$z[df2$cnd] <- bp_tbl$z
      df2
    }
  )


  expect_equal(
    dplyr::mutate(df,
      cnd = r_lgl(.seed = 99186381),
      bp_where(cnd, bp, .seed = 11311)
    ),
    {
      df2 <- dplyr::mutate(df, cnd = r_lgl(.seed = 99186381), x = NA, y = NA, z = NA)
      bp_tbl <- bp(n = sum(df2$cnd), .seed = 11311)
      df2$x[df2$cnd] <- bp_tbl$x
      df2$y[df2$cnd] <- bp_tbl$y
      df2$z[df2$cnd] <- bp_tbl$z
      df2
    }
  )
})

test_that("bp_where errors correctly", {
  expect_error(
    bp_where("test", bp)
  )
  expect_error(
    bp_where(rep(T, 5), bp(n = 10))
  )
})

