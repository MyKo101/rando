
test_that("logit() and invlogit() output values correctly", {
  expect_equal(
    logit(0.5),
    0
  )
  expect_equal(
    logit(0.1),
    -2.19722457733622
  )
  expect_equal(
    logit(0.9),
    2.19722457733622
  )
  expect_equal(
    logit(0.1, base = 10),
    -0.954242509439325
  )
  expect_equal(
    logit(0.9, base = 2),
    3.16992500144231
  )

  expect_equal(
    logit(3:5 / 10),
    c(-0.847297860387204, -0.405465108108164, 0)
  )


  expect_equal(
    invlogit(0),
    0.5
  )

  expect_equal(
    invlogit(2),
    0.880797077977882
  )

  expect_equal(
    invlogit(-2),
    0.119202922022118
  )


  expect_equal(
    invlogit(2, base = 10),
    0.99009900990099
  )

  expect_equal(
    invlogit(-2, base = 4),
    0.0588235294117647
  )

  expect_equal(
    invlogit(-1:1),
    c(0.268941421369995, 0.5, 0.731058578630005)
  )
})

test_that("logit() and invlogit() invert eachother", {
  test_vals <- r_unif(n = 100, .seed = 76418)
  test_vals <- test_vals[0 < test_vals & test_vals < 1]

  for (x in test_vals) {
    expect_equal(
      invlogit(logit(x)),
      x
    )
  }

  test_vals2 <- r_norm(n = 100, .seed = 178115)

  for (x in test_vals) {
    expect_equal(
      logit(invlogit(x)),
      x
    )
  }
})

test_that("logit() and invlogit() error correctly", {
  expect_error(
    logit(-1)
  )

  expect_error(
    logit(1.5)
  )

  expect_error(
    logit(0.5, base = -1)
  )

  expect_error(
    logit(0.5, base = 1)
  )

  expect_error(
    invlogit(0.5, base = -1)
  )
})
