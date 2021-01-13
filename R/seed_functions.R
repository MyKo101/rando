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
#'
NULL


#' @describeIn seed Generates a random seed, which can be used in \code{set_seed()}
#'
#' @export
#'
#' @examples
#' my_seed <- gen_seed()
gen_seed <- function() stats::runif(1, 1 - 2^31, 2^31 - 1)

#' @describeIn  seed Sets the current seed
#'
#' @param seed
#' The random seed to be used
#'
#' @export

set_seed <- function(seed) {
  check_must_be_between(seed, 1 - 2^31, 2^31 - 1)
  set.seed(seed)
}

#' @describeIn seed Evaluates the expression after setting the seed.
#' If \code{seed} is
#' \code{TRUE}, then it first generates a feasible random seed. Results
#' are output with the \code{seed} attached (if set), which can be later
#' extracted with \code{pull_seed()}
#'
#'
#' @param expression
#' expression to be evaluated
#'
#' @export
#'
#' @examples
#' res <- with_seed(my_seed, r_norm(n = 10))
#' res
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
#' \code{with_seed}
#'
#' @param x
#' object to extract the \code{seed} from
#'
#' @export
#'
#' @examples
#' pull_seed(res)
pull_seed <- function(x) attr(x, "seed")
