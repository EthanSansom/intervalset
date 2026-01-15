# todos ------------------------------------------------------------------------

# TODO: Rename `sizes` to `size`. `starts` and `ends` make sense, because it's
# per set, but `size` refers to the size of each element.

# TODO: better phinterval constructor
# - Supply parallel date vectors and the indices/groups along which to split them
# - Use `sizes` (from `vctrs::list_unchop`) and `groups` (from `base::split(f = groups)`)
# - Matches: lubridate::interval(start = NULL, end = NULL, tzone = tz(start))
# phinterval <- function(
#     starts = NULL,
#     ends = NULL,
#     tzone = tz(start),
#     groups = NULL,
#     groups_na_rm = TRUE
# ) {
#   # 1. Verify that starts, ends, tzone, sizes, groups are valid
#   # 2. Flip the starts/ends so they're not reversed
#
#   # 3A. Simple case where we're making an interval vector
#   if (is.null(sizes) && is.null(groups)) {
#     new_phinterval(
#       starts = as.numeric(starts),
#       ends = as.numeric(ends),
#       sizes = rep(1L, length(starts))
#     )
#   }
#
#   # 3B. Deal with the `sizes` or `groups`
#   # - We'll have to deal with squashing within-group, since the date ranges
#   #   aren't guaranteed to be sorted or non-overlapping
#   #   - Probably a custom C++ squash
#   #   - Maybe only allow `groups`
#   if (!is.null(groups)) {
#     if (
#       !vctrs::obj_is_vector(groups) ||
#       vctrs::vec_any_missing(groups) ||
#       length(groups) != length(starts)
#     ) {
#       # Error: We'll only allow vectors in groups, with no missing values, and
#       #        of the same length as the input interval.
#     }
#     group_index <- vctrs::vec_duplicate_id(groups)
#
#     # Depending on `groups_na_rm`, we'll need to remove NA values prior to splitting
#     # or we just have a policy of not accepting NA values (weird).
#
#     #...?
#   }
# }

# TODO: Not using {vctrs}
# This is so un-vector-like that I'm just going to use {vctrs} for the proxy
# and restoration process. Resources:
# - {lubridate} compatibility with vctrs: https://github.com/tidyverse/lubridate/blob/main/R/vctrs.R
# - The "vctrs_vctr" methods: https://github.com/r-lib/vctrs/blob/main/R/type-vctr.R
# - S3 Ops group generic: https://adv-r.hadley.nz/s3.html#group-generics
# - Interval methods definition (although S4): https://github.com/tidyverse/lubridate/blob/main/R/intervals.r
#
# We'll need to implement vec_proxy, vec_restore, and vec_cast for <phinterval>
# and <Interval>.

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
new_iset1 <- function(sizes, starts, ends) {
  vctrs::new_vctr(
    sizes,
    starts = starts,
    ends = ends,
    class = "iset1"
  )
}

#' @export
is_iset1 <- function(x) {
  inherits(x, "iset1")
}

#' @export
format.iset1 <- function(x, ...) {
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
vec_ptype2.iset1.iset1 <- function(x, y, ...) new_iset1()

#' @export
vec_cast.iset1.iset1 <- function(x, to, ...) x

# HACK: See `vec_restore.iset1()` for details.
#' @export
vec_cast.iset1.data.frame <- function(x, to, ...) {
  stop_incompatible_cast(x, to, x_arg = "x", to_arg = "to")
}

# TODO: Look at which `.vctrs_vctr` methods call the proxy (e.g. length()).
#       The proxy is slow, so we should avoid it where possible.
#' @export
vec_proxy.iset1 <- function(x, ...) {
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
vec_restore.iset1 <- function(x, to, ...) {
  # HACK:
  # `vctrs::vec_default_cast(x, to)` calls `vec_restore(x, to)` within function
  # `vctrs:::vctr_cast()`. This means that `vec_cast(10, new_iset1())` will
  # call `vec_restore(10, new_iset1())` which causes an error, because `10`
  # is not a valid proxy. To prevent this, we check whether `x` is a data frame
  # (e.g. a proxy) here and override `vec_cast.iset1.data.frame` manually to
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
  new_iset1(
    sizes = x[["sizes"]],
    starts = listof_dbl_unchop_cpp(x[["starts"]]),
    ends = listof_dbl_unchop_cpp(x[["ends"]])
  )
}

# The `[<-.vctrs_vctr` method already uses `vec_slice()`. For the remaining
# sub-setting methods, we need to ensure that the proxy is used.
#' #' @export
`[<-.iset1` <- function(x, i, value, ...) {
  vctrs::vec_assign(x, i, value)
}

#' @export
`[[.iset1` <- function(x, i, ...) {
  i <- vctrs::vec_as_location2(i, n = length(x), names = names(x))
  vctrs::vec_slice(x, i)
}

#' @export
`[[<-.iset1` <- function(x, i, value, ...) {
  i <- vctrs::vec_as_location2(i, n = length(x), names = names(x))
  vctrs::vec_assign(x, i, value)
}

# NOTE: When an interval set is `NA` the starts/ends list columns contains `NA_real_`
#       and not `NULL`. So `NA` proxy rows aren't considered `NA` by {vctrs}.
#' @export
is.na.iset1 <- function(x) {
  is.na(unclass(x))
}

#' @export
length.iset1 <- function(x) {
  length(unclass(x))
}

# set operations ---------------------------------------------------------------

#' @export
intersect_v1 <- function(x, y) {
  stopifnot(is_iset1(x), is_iset1(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  out <- intersect_v1_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )

  new_iset1(
    out[["sizes"]],
    out[["starts"]],
    out[["ends"]]
  )
}

#' @export
squash_v1 <- function(x, na.rm = TRUE) {
  stopifnot("`x` must be an <iset1>." = is_iset1(x))

  has_nas <- anyNA(unclass(x))
  if (!na.rm && has_nas) {
    return(new_iset1(NA_integer_, NA_real_, NA_real_))
  }
  if (length(unclass(x)) == 0) {
    return(new_iset1(integer(), numeric(), numeric()))
  }

  if (has_nas) {
    starts <- attr(x, "starts")
    ends <- attr(x, "ends")
    nas <- is.na(starts)
    out <- squash_v1_cpp(starts = starts[!nas], ends = ends[!nas])
  } else {
    out <- squash_v1_cpp(starts = attr(x, "starts"), ends = attr(x, "ends"))
  }

  new_iset1(out[["sizes"]], out[["starts"]], out[["ends"]])
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
