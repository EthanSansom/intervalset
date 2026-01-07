# TODO:
# - Remove exports and references to {generics}, {rlang}, and {vctrs} (switch to base only)
# - Add unit tests

# class ------------------------------------------------------------------------

#' @export
new_intervalset <- function(list_of_matrices) {
  vctrs::new_vctr(list_of_matrices, class = "intervalset")
}

#' @export
is_intervalset <- function(x) {
  inherits(x, "intervalset")
}

#' @export
is_emptyset <- function(x) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  lengths(x) == 0L
}

#' @export
is.na.intervalset <- function(x) {
  vapply(x, \(ivs) length(ivs) && is.na(ivs[[1]]), logical(1L))
}

#' @export
format.intervalset <- function(x, ...) {
  # TODO: In the {phinterval} case, we'd convert the starts and ends to
  #       POSIXct (origin = "1970-01-01") so that they'd be formatted as
  #       times rather than as numbers.
  out <- vapply(
    x,
    \(ivs) {
      paste0(
        "{",
        paste0("[", starts(ivs), ", ", ends(ivs), "]", collapse = " U "),
        "}"
      )
    },
    FUN.VALUE = character(1)
  )
  out[is_emptyset(x)] <- "{}"
  out[is.na(x)] <- NA_character_
  out
}

# set operations ---------------------------------------------------------------

#' @export
squash <- function(x, na.rm = TRUE) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  stopifnot("`na.rm` must be `TRUE` or `FALSE`." = is.logical(na.rm) && length(na.rm) == 1 && !is.na(na.rm))

  new_intervalset(list(
    squash_interval_set(do.call(rbind, vctrs::vec_data(x)), na.rm)
  ))
}

#' @export
intervalset_intersect <- function(x, y) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  stopifnot("`y` must be an <intervalset>." = is_intervalset(y))

  new_intervalset(
    v_intersect_interval_set(vctrs::vec_data(x), vctrs::vec_data(y))
  )
}

#' @export
intervalset_union <- function(x, y) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  stopifnot("`y` must be an <intervalset>." = is_intervalset(y))

  new_intervalset(
    v_union_interval_set(vctrs::vec_data(x), vctrs::vec_data(y))
  )
}

#' @export
intervalset_setdiff <- function(x, y) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  stopifnot("`y` must be an <intervalset>." = is_intervalset(y))

  new_intervalset(
    v_intersect_intervalset(
      vctrs::vec_data(x),
      v_compliment_interval_set(vctrs::vec_data(y))
    )
  )
}

#' @export
intervalset_overlaps <- function(x, y) {
  stopifnot("`x` must be an <intervalset>." = is_intervalset(x))
  stopifnot("`y` must be an <intervalset>." = is_intervalset(y))

  v_overlaps_interval_set(vctrs::vec_data(x), vctrs::vec_data(y))
}

# utils ------------------------------------------------------------------------

starts <- function(ivs) {
  ivs[ , 1]
}

ends <- function(ivs) {
  ivs[ , 2]
}
