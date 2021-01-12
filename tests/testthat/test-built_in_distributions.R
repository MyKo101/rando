set_n(100)

test_that("r_norm works", {

  #Check we're getting the default length
  expect_length(r_norm(),100)

  #Check we can overwrite the length manually
  expect_length(r_norm(n=50),50)

  #Check the length can be extracted from arguments
  expect_length(r_norm(mean=c(-10,10)),2)
  expect_length(r_norm(sd=c(1,2)),2)


  #Check invalid parameters throw errors
  expect_error(r_norm(sd=-1))
  expect_error(r_norm(sd=0))
  expect_error(r_norm(n=-1))
  expect_error(r_norm(n=c(1,2)))
  expect_error(r_norm(n=1.5))
  expect_error(r_norm(mean=c(1,2,3),sd=c(0.5,1)))



})

