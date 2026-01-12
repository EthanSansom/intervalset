# todos ------------------------------------------------------------------------

# TODO: Custom `c()`
# Implement a custom `c()` and `vec_c()` (if able) implementation. We don't need
# the proxy, because combining multiple interval-sets just involves concatenating
# their starts, ends, and sizes.
#
# E.g. c(x, y) -> sizes = c(x$sizes, y$sizes), starts = c(x$starts, y$starts), etc.
#
# So, we only need to convert <Interval> vectors to the correct sizes, starts, ends
# and then concatenate.

# TODO: Dealing with `vec_proxy()`
# This is looking like we'll want to circumvent `vec_proxy()` everywhere, and
# use the current `vec_proxy()` only for `vec_proxy_equal()`. Or, maybe just
# leave the slow `vec_proxy()`, so this works as expected (but slower) for
# {vctrs} specific operations (e.g. vec_size(), vec_c(), etc.)

# TODO: Custom `[<-` and `[[<-`
# Manual multiple dispatch:
# - if (is.logical(value) && allNA(value)) { handle special NA case }
# - if (is.interval(value)) { lubridate <Interval> case }
# - if (is_phinterval(value)) { <phinterval> case }
#
sub_assign <- function(x, i, value) {
  # Let {vctrs} handle converting `i` to an integer vector
  i <- vctrs::vec_as_location(i, n = length(x), names = names(x))

  # Check size compatibility in R
  stopifnot(length(value) == length(i) || length(value) == 1L)

  # In C++, we'll:
  # - Calculate the offsets in x using `sizes`
  # - Allocate new sizes, starts, ends.
  #   - For sizes, just duplicate `x` sizes
  #   - For starts and ends, you can calculate the required size
  # - Iterate along x.
  #   - If `index != i` then assign the original starts/ends of `x`
  #   - If `index == i` then assign the new sizes/starts/ends from `value`

  # In C++, we'll give the sorted `i` indices. Since they're sorted, we just
  # check whether each `i` is equal to the current index.
  #
  # int n_idx = indices.size();
  # int current_pos = 0;
  #
  # for (int i = 0; i < n; i++) {
  #   if (current_pos < n_idx && i == indices[current_pos]) {
  #     // do something
  #     current_pos++;
  #   } else {
  #     // do something else
  #   }
  # }
}
sub_set <- function(x, i) {
  i <- vctrs::vec_as_location(i, n = length(x), names = names(x))

  # int n_sets = indices.size();
  # IntegerVector out_sizes = no_init(n_sets);
  #
  # // Calculate the number of spans in the output vector and assign sizes
  # int n_spans = 0;
  # for (int i = 0; i < n_sets; i++) {
  #   int index = indices[i];
  #
  #   out_sizes[i] = x_sizes[index];
  #   n_spans += x_sizes[index];
  # }
  # NumericVector out_starts = no_init(n_spans);
  # NumericVector out_ends = no_init(n_spans);
  #
  # // Sub-Assign
  # // TODO: Think about this, there should be an efficient way to pull elements
  # //       out of `x` sizes, starts, and ends. Using the sizes as offsets.
}

# class ------------------------------------------------------------------------

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
is_iset <- function(x) {
  inherits(x, "iset")
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
  sizes_chop <- na_to_one(sizes)

  names(sizes) <- names(x) # TODO: Do we need this and does it preserve names?
  new_data_frame(list(
    sizes = sizes,
    starts = vec_chop(attr(x, "starts"), sizes = sizes_chop),
    ends = vec_chop(attr(x, "ends"), sizes = sizes_chop)
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
    starts = listof_dbl_unchop_cpp(x[["starts"]]),
    ends = listof_dbl_unchop_cpp(x[["ends"]])
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

# NOTE: When an interval set is `NA` the starts/ends list columns contains `NA_real_`
#       and not `NULL`. So `NA` proxy rows aren't considered `NA` by {vctrs}.
#' @export
is.na.iset <- function(x) {
  is.na(unclass(x))
}

# set operations ---------------------------------------------------------------

new_intersect <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  out <- new_intersect_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
  new_iset(out[["sizes"]], out[["starts"]], out[["ends"]])
}

# utils ------------------------------------------------------------------------

na_to_one <- function(x) {
  x[is.na(x)] <- 1L
  x
}

unattr <- function(x) {
  attributes(x) <- NULL
  x
}

# comparison helpers -----------------------------------------------------------

isets_compare <- function(old_iset, new_iset) {
  stopifnot(inherits(old_iset, "iset0"), inherits(new_iset, "iset"))
  waldo::compare(as_new_iset(old_iset), new_iset)
}

as_old_iset <- function(x) {
  stopifnot(inherits(x, "iset"))

  new_iset0(
    sizes = unattr(unclass(x)),
    starts = attr(x, "starts"),
    ends = attr(x, "ends")
  )
}

as_new_iset <- function(x) {
  stopifnot(inherits(x, "iset0"))

  sizes <- purrr::map_int(x, ~ ifelse(is.null(.x), NA_integer_, nrow(.x)))
  starts <- purrr::map(x, ~ if (is.null(.x)) { NA_real_ } else { .x[, 1] }) |> unlist()
  ends <- purrr::map(x, ~ if (is.null(.x)) { NA_real_ } else { .x[, 2] }) |> unlist()

  new_iset(sizes, starts, ends)
}
