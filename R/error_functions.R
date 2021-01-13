

error_glue <- function(..., .envir = parent.frame()) {
  error_message <- paste0(unlist(list(...)), collapse = "")

  error_message <- glue::glue(error_message, .envir = .envir)
  error_message < paste0(error_message, collapse = " ")
  stop(error_message, call. = F)
}


check_error <- function(criteria) {
  f_nm <- match.call2(2)[[1]]
  v_nm <- match.call2(1)[["value"]]
  criteria2 <- glue::glue(criteria, .envir = parent.frame())
  error_glue("{v_nm} provided to {f_nm}() must be {criteria2}")
}


check_n <- function(value) {
  if (!(length(value) == 1 &&
    is_wholenumber(value) && value >= 0)) {
    check_error("a single positive whole number")
  }
}


check_must_be_positive <- function(value) {
  if (!all(value >= 0)) check_error("positive")
}

check_must_be_strictly_positive <- function(value) {
  if (!all(value > 0)) check_error("strictly positive")
}

check_must_be_between <- function(value, lower, upper) {
  if (!all(lower <= value & value <= upper)) check_error("between {lower} and {upper}")
}

check_must_be_strictly_between <- function(value, lower, upper) {
  if (!all(lower < value & value < upper)) check_error("strictly between {lower} and {upper}")
}

check_must_be_integer <- function(value) {
  if (!all(is_wholenumber(value))) check_error("integer")
}
