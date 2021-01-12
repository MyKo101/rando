
#' @name r_cdf
#'
#' @title Generate Random Numbers Based on an arbitrary CDF
#'
#' @description
#' Generates Random Numbers based on a distribution defined by any
#' arbitrary cumulative distribution function
#'
#' @param Fun
#' function to use as the cdf. See details
#'
#' @param min,max
#' range values for the domain of the `Fun`
#'
#' @param ...
#' arguments that can be passed to `Fun`
#'
#' @details
#' The `Fun` argument accepts `purrr` style inputs.
#' Must be vectorised, defined on the whole Real line and return a
#' single numeric value between 0 and 1 for any input. The random
#' variable will be passed to `Fun` as the first argument.
#' This means that R's argument matching can be used with named
#' arguments in `...` if a different positional argument is wanted.
#'
#' @param data
#' data set containing arguments to be passed to  `Fun`
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' my_fun <- function(x,beta=1){
#'   1-exp(-beta*x)
#' }
#'
#' r_cdf(my_fun)
#'
#'
#' r_cdf(~1-exp(-.x),min=0)
#'
#' r_cdf(~1-exp(-.x*beta),beta=1:10,min=0)
#'
#'
#'
#' @export
r_cdf <- function(Fun,min=-Inf,max=Inf,...,data=NULL,n=default_n(...,data[[1]]),.seed=NULL){
  check_n(n)

  if(!is.null(data) && !is.data.frame(data)){
    data <- as.data.frame(data)
  }

  dots <- c(list(...),as.list(data))
  dots <- dots[names(dots) != ""]



  with_seed(
    .seed,
    invert_Fun(u = stats::runif(n=n,min=0,max=1),
               Fun = Fun,
               min = min,
               max = max,
               args = dots)
  )
}

invert_Fun <- function(u,Fun,min,max,args=NULL){
  list2env(args,environment())


  .Fun <- as_function(Fun)

  .Fun_call <- as.call(c(quote(.Fun),quote(.est),args))
  n_out <- length(u)

  lower_bound <- if(is.infinite(min)) {
    invert_Fun_lower_bound(u,.Fun,args)
  } else {
    rep(min,n_out)
  }

  upper_bound <- if(is.infinite(max)) {
    invert_Fun_upper_bound(u,.Fun,args)
  } else {
    rep(max,n_out)
  }


  while(any(upper_bound - lower_bound > 10^(-5)))
  {
    .est <- (lower_bound + upper_bound)/2

    current_u <- eval(.Fun_call)

    too_low <- current_u < u

    lower_bound[too_low] <- .est[too_low]
    upper_bound[!too_low] <- .est[!too_low]

  }

  .est

}

invert_Fun_lower_bound <- function(u,.Fun,args){
  list2env(args,environment())
  .Fun_call <- as.call(c(quote(.Fun),quote(.est),args))

  n_out <- length(u)
  lower_bound <- rep(0,n_out)
  not_done <- rep(TRUE,n_out)

  while(any(not_done))
  {
    .est <- lower_bound[not_done]

    current_u <- eval(.Fun_call)

    too_high <- u[not_done] < current_u

    lower_bound[not_done][too_high] <- lower_bound[not_done][too_high] - 100
    not_done[not_done] <- not_done[not_done] & too_high
  }

  lower_bound
}

invert_Fun_upper_bound <- function(u,.Fun,args){
  list2env(args,environment())
  .Fun_call <- as.call(c(quote(.Fun),quote(.est),args))

  n_out <- length(u)
  upper_bound <- rep(0,n_out)
  not_done <- rep(TRUE,n_out)

  while(any(not_done))
  {
    .est <- upper_bound[not_done]

    current_u <- eval(.Fun_call)


    too_low <- current_u < u[not_done]

    upper_bound[not_done][too_low] <- upper_bound[not_done][too_low] + 100
    not_done[not_done] <- not_done[not_done] & too_low
  }

  upper_bound
}

cat0 <- function(...){
  cat("\n",...,"\n")
}


