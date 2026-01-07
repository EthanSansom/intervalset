#' @export
new_iset <- function(sizes, starts, ends) {
  vctrs::new_vctr(
    sizes,
    starts = starts,
    ends = ends,
    class = "iset"
  )
}

#' @export
format.iset <- function(x, ...) {
  sizes <- unattr(unclass(x))
  sizes[is.na(sizes)] <- 1L

  starts <- attr(x, "starts")
  ends <- attr(x, "ends")

  out <- paste0("{", purrr::map2_chr(
    vctrs::vec_chop(starts, sizes = sizes),
    vctrs::vec_chop(ends, sizes = sizes),
    \(starts, ends) paste(paste0("[", starts, ", ", ends, "]"), collapse = ", ")
  ), "}")

  out[!sizes] <- "{}"
  out[is.na(x)] <- NA_character_
  out
}

#' @export
vec_ptype2.iset.iset <- function(x, y, ...) new_iset()

#' @export
vec_cast.iset.iset <- function(x, to, ...) x

# HACK: See `vec_restore.iset()` for details.
#' @export
vec_cast.iset.data.frame <- function(x, to, ...) {
  stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to")
}

# TODO: Look at which `.vctrs_vctr` methods call the proxy (e.g. length()).
#       The proxy is slow, so we should avoid it where possible.

#' @export
vec_proxy.iset <- function(x, ...) {
  sizes <- unattr(unclass(x))
  names(sizes) <- names(x) # TODO: Do we need this and does it preserve names?

  new_data_frame(list(
    sizes = sizes,
    starts = starts_list(x),
    ends = ends_list(x)
  ))
}

#' @export
vec_restore.iset <- function(x, to, ...) {
  # HACK:
  # `vctrs::vec_default_cast(x, to)` calls `vec_restore(x, to)` within function
  # `vctrs:::vctr_cast()`. This means that `vec_cast(10, new_iset())` will
  # call `vec_restore(10, new_iset())` which causes an error, because `10`
  # is not a valid proxy. To prevent this, we check whether `x` is a data frame
  # (e.g. a proxy) here and override `vec_cast.iset.data.frame` manually to
  # prevent casts with `x = <data.frame>`.
  #
  # There is a fixme to prevent this behavior in `vec_default_cast()`, see:
  # https://github.com/r-lib/vctrs/blob/788f435796383833c4d54735948f3f13f9376751/R/type-vctr.R#L142
  if (!is.data.frame(x)) {
    stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to")
  }

  # TODO: What other attributes do we need to restore here?
  # - What about user-supplied attributes!
  # - Can we attach all required attributes to `sizes` in the proxy?
  # - Does attaching attributes impact equality? If so, define a separate `vec_proxy_equals()`.
  new_iset(
    sizes = x[["sizes"]],
    starts = list_of_numeric_unchop_cpp(x[["starts"]]),
    ends = list_of_numeric_unchop_cpp(x[["ends"]])
  )
}

# The `[<-.vctrs_vctr` method already uses `vec_slice()`. For the remaining
# sub-setting methods, we need to ensure that the proxy is used.
#' #' @export
`[<-.iset` <- function(x, i, value, ...) {
  vctrs::vec_assign(x, i, value)
}

#' @export
`[[.iset` <- function(x, i, ...) {
  i <- vctrs::vec_as_location2(i, n = length(x), names = names(x))
  vctrs::vec_slice(x, i)
}

#' @export
`[[<-.iset` <- function(x, i, value, ...) {
  i <- vctrs::vec_as_location2(i, n = length(x), names = names(x))
  vctrs::vec_assign(x, i, value)
}

starts_list <- function(x) {
  sizes <- unattr(unclass(x))
  sizes[is.na(sizes)] <- 1L
  vctrs::vec_chop(attr(x, "starts"), sizes = sizes)
}

ends_list <- function(x) {
  sizes <- unattr(unclass(x))
  sizes[is.na(sizes)] <- 1L
  vctrs::vec_chop(attr(x, "ends"), sizes = sizes)
}

points_unchop <- function(x) {
  null_at <- vapply(x, is.null, TRUE)
  x[null_at] <- list(NA_real_)
  unlist(x)
}

unattr <- function(x) {
  attributes(x) <- NULL
  x
}
