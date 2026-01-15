# class ------------------------------------------------------------------------

# NOTE: Initiates the same as `new_iset()`, converts to list of matrix representation.
#' @export
new_iset0 <- function(sizes, starts, ends) {
  sizes_chop <- sizes
  sizes_chop[is.na(sizes_chop)] <- 1L

  starts <- vec_chop(starts, sizes = sizes_chop)
  ends <- vec_chop(ends, sizes = sizes_chop)

  matrices <- purrr::map2(starts, ends, ~matrix(c(.x, .y), ncol = 2L))
  matrices[is.na(sizes)] <- list(NULL)
  new_iset0_impl(matrices)
}

new_iset0_impl <- function(list_of_matrices) {
  vctrs::new_vctr(list_of_matrices, class = "iset0")
}

#' @export
is_iset0 <- function(x) {
  inherits(x, "iset0")
}

#' @export
format.iset0 <- function(x, ...) {
  fmt <- function(x) {
    paste0("{", paste0("[", x[, 1], ", ", x[, 2], "]", collapse = ", "), "}")
  }
  is_hole <- function(x) purrr::map_lgl(x, ~ is.matrix(.x) && length(.x) == 0L)

  out <- purrr::map_chr(x, fmt)
  out[is_hole(x)] <- "{}"
  out[is.na(x)] <- NA_character_
  out
}

# set operations ---------------------------------------------------------------

#' @export
intersect_v0 <- function(x, y) {
  stopifnot("`x` must be an <iset0>." = is_iset0(x))
  stopifnot("`y` must be an <iset0>." = is_iset0(y))
  sets <- vec_recycle_common(x, y)

  new_iset0_impl(
    intersect_v0_cpp(
      vctrs::vec_data(sets[[1]]),
      vctrs::vec_data(sets[[2]])
    )
  )
}

#' @export
squash_v0 <- function(x, na.rm = TRUE) {
  stopifnot("`x` must be an <iset0>." = is_iset0(x))
  any_na <- anyNA(x)
  if (any_na && !na.rm) {
    return(new_iset0(NA_integer_, NA_real_, NA_real_))
  }
  if (length(x) == 0) {
    return(new_iset0(integer(), numeric(), numeric()))
  }

  # Collapse the matrices to obtain a single interval set. This removes NA's.
  interval_set <- do.call(rbind, vec_data(x))
  if (is.null(interval_set)) {
    return(new_iset0(NA_integer_, NA_real_, NA_real_))
  }
  new_iset0_impl(list(squash_v0_cpp(interval_set)))
}
