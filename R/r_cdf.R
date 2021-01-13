
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
#' my_fun <- function(x, beta = 1) {
#'   1 - exp(-beta * x)
#' }
#'
#' r_cdf(my_fun)
#'
#'
#' r_cdf(~ 1 - exp(-.x), min = 0)
#'
#' r_cdf(~ 1 - exp(-.x * beta), beta = 1:10, min = 0)
#' @export
r_cdf <- function(Fun, min = -Inf, max = Inf, ..., data = NULL, n = default_n(..., data), .seed = NULL) {
  check_n(n)

  dots <- list(...)
  if (!is.null(data)) {
    dots <- c(dots, as.list(data))
  }

  dots <- dots[names(dots) != ""]

  with_seed(
    .seed,
    invert_Fun(
      u = stats::runif(n = n, min = 0, max = 1),
      Fun = Fun,
      min = min,
      max = max,
      args = dots
    )
  )
}

invert_Fun <- function(u, Fun, min, max, args = NULL) {
  .Fun <- as_function(Fun)
  .Fun_call <- as.call(c(quote(.Fun), quote(.est)))

  if (length(args) > 0) {
    list2env(args, environment())
    arg_nms <- names(args)

    if (length(args) > 0) {
      for (i in 1:length(args)) {
        .Fun_call[[arg_nms[i]]] <- str2lang(arg_nms[i])
      }
    }
  }

  n_out <- length(u)

  lower_bound <- if (is.infinite(min)) {
    invert_Fun_find_bound(u, .Fun, args, "lower")
  } else {
    rep(min, n_out)
  }

  upper_bound <- if (is.infinite(max)) {
    invert_Fun_find_bound(u, .Fun, args, "upper")
  } else {
    rep(max, n_out)
  }


  while (any(upper_bound - lower_bound > 10^(-5))) {
    .est <- (lower_bound + upper_bound) / 2

    current_u <- eval(.Fun_call)

    too_low <- current_u < u

    lower_bound[too_low] <- .est[too_low]
    upper_bound[!too_low] <- .est[!too_low]
  }

  .est
}


invert_Fun_find_bound <- function(u, .Fun, args, type = c("upper", "lower")) {
  .Fun_call <- as.call(c(quote(.Fun), quote(.est)))

  if (length(args) > 0) {
    list2env(args, environment())
    arg_nms <- names(args)
    args_i <- lapply(paste0(arg_nms, "[not_done]"), str2lang)
    for (i in 1:length(arg_nms)) {
      .Fun_call[[i + 2]] <- args_i[[i]]
      names(.Fun_call)[i + 2] <- arg_nms[i]
    }
  }


  n_out <- length(u)
  bound <- rep(0, n_out)
  not_done <- rep(TRUE, n_out)

  while (any(not_done)) {
    .est <- bound[not_done]

    current_u <- eval(.Fun_call)

    if (type == "upper") {
      out_of_bound <- current_u < u[not_done]
      bound[not_done][out_of_bound] <- bound[not_done][out_of_bound] + 100
    } else {
      out_of_bound <- u[not_done] < current_u
      bound[not_done][out_of_bound] <- bound[not_done][out_of_bound] - 100
    }

    not_done[not_done] <- not_done[not_done] & out_of_bound
  }

  bound
}
