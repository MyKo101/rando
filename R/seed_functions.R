#' @name seed
#'
#' @title Random Seed Defining Functions
#'
#' @description
#' Functions related to generating random seeds and utilising
#' them for reproducibility.
#'
#' @details
#' Random values are generated based on the current seed used by
#' the R system. This means by deliberately setting a seed in R,
#' we can make work reproducible.
#'
#' @return
#'
#' `gen_seed()` returns a single numeric value
#'
#' `with_seed()` returns the value of the evaluated expression after
#' with the relevant seed as an attribute (if required)
#'
#' `pull_seed()` returns a single numeric value
#'
#' `fix_seed()` and `set_seed()` do not return anything
#'
NULL


#' @describeIn seed Generates a random seed, which can be used in \code{set_seed()}
#'
#' @examples
#' my_seed <- gen_seed()
#'
#' @export
#'
gen_seed <- function() stats::runif(1, 1 - 2^31, 2^31 - 1)

#' @describeIn  seed Sets the current seed
#'
#' @param seed
#' The random seed to be used
#'
#' @examples
#'
#' set_seed(my_seed)
#' r_norm(n=10)
#' set_seed(my_seed)
#' r_norm(n=10)
#'
#' @export
#'
set_seed <- function(seed) {
  check_must_be_between(seed, 1 - 2^31, 2^31 - 1)
  set.seed(seed)
}

#' @describeIn seed Resets the seed to re-run code
#'
#' @param reset
#' Should the fixed seed be forced to reset
#'
#' @examples
#'
#' fix_seed()
#' r_norm(n=3)
#'
#' fix_seed()
#' r_norm(n=3)
#'
#' fix_seed(reset=TRUE)
#' r_norm(n=3)
#'
#' @export
#'
fix_seed <- function(reset=FALSE){
  if(reset){
    seed <- gen_seed()
  } else {
    seed <- getOption("rando.fixed.seed",default=gen_seed())
  }
  options(rando.fixed.seed = seed)
  set_seed(seed)
}

#' @describeIn seed Evaluates the expression after setting the seed.
#' If `seed` is `TRUE`, then it first generates a seed using
#' `gen_seed()`. Results are output with the `seed` attached (if set).#'
#'
#' @param expression
#' expression to be evaluated
#'
#' @examples
#'
#' res <- with_seed(my_seed, r_norm(n = 10))
#' res
#'
#' @export
#'
with_seed <- function(seed, expression) {
  .expression <- substitute(expression)
  if (is.null(seed)) {
    eval.parent(.expression)
  } else {
    if (is.logical(seed) && isTRUE(seed)) {
      seed <- gen_seed()
    } else if (!is.numeric(seed)) {
      error_glue(
        "Non-compatible .seed provided.",
        "It must be either TRUE or numeric"
      )
    }
    set_seed(seed)
    structure(
      eval.parent(.expression),
      seed = seed
    )
  }
}



#' @describeIn seed Extracts the seed used to generate the results of
#' `with_seed()`
#'
#' @param x
#' object to extract the `seed` from
#'
#' @examples
#'
#' pull_seed(res)
#'
#' @export
#'
pull_seed <- function(x) attr(x, "seed")
