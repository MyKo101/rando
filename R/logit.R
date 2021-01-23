#' @name logit
#'
#' @title The logit and inverse logit functions
#'
#' @description
#' Calculates the logit or the inverse logit of a value
#'
#' @param prob
#' vector of probabilities
#'
#' @param base
#' base of the logarithmic function to use
#'
#' @return
#' A numeric vector
#'
#' @examples
#'
#' logit(0.5)
#'
#' logit(seq(0.01, 0.99, 0.01))
#' @export

logit <- function(prob, base = exp(1)) {
  check_must_be_strictly_between(prob, 0, 1)
  check_must_be_strictly_positive(base)
  if (any(base == 1)) error_glue("base cannot be 1 in logit()")
  log(prob / (1 - prob), base = base)
}

#' @rdname logit
#'
#' @param alpha
#' vector of values to find the inverse logit of
#'
#' @examples
#' invlogit(-10:10)
#' @export
invlogit <- function(alpha, base = exp(1)) {
  check_must_be_strictly_positive(base)
  alogb <- alpha * log(base)

  exp(alogb) / (1 + exp(alogb))
}
