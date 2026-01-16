# todos ------------------------------------------------------------------------

# TODO: Formatting
#
# I have three width options:
# 1. <phint[3]>
# 2. { 2026-01-15-[3]-2026-01-26 }
# 3. { 2026-01-15--2026-01-16, 2026-01-18--2026-01-20, 2026-01-25--2026-01-26 }
#
# 4A. Single elements, "{ 2026-01-15--2026-01-26 }"
# 4B. NA elements, <NA>
# 4C. Holes, <hole>
#
# (1) is likely to be shown in a tibble, (2) is the default print (looks like an
# <Interval>), and (3) is when the user asks "I'd like to see all the data".
#
# - Implement custom pillar options for switching between (1) and (2)
#   - In pillar shaft, drop the "{}" to save space
# - Look into how other packages implement a `max_print_width`
# - Choose between (2) and (3) dynamically when printing to the console
#
# Options:
# - phinterval.print_max_width  -> printing options, toggle between 1-3
# - phinterval.pillar_max_width -> printing in tibbles, toggle between 1-3 (1-2?)

# TODO: Object naming scheme:
# - phint_operate(phint, phint, fn) -> parallel, returns a phinterval
# - phint_relate(phint, phint, fn)  -> parallel, returns a logical
# - phint_modify(phint, fn)         -> single, returns a phinterval
# - int_operate/relate/modify()     -> same, input is an interval, fast track
#
# - PhintervalVector -> AKA SpanSetVector ({size, starts, ends})
# - IntervalVector   -> ({start, end})

# TODO: Remaining functionality
# - operations: union, setdiff
# - modifications: invert, complement, sift
# - relations: within, overlaps

# TODO: as.character
# If someone wants to do the "I'd like to see everything" option, do as.character().
# For this purpose, we'll also allow an `as_phinterval.character` method so you
# can make the round trip. Unfortunately, that means storing a timezone somewhere.
#
# You can use `lubridate::ymd_hms()` or other parser for reading the input, but
# you can't make any nice assumptions about the input dates, so you'll have to
# use `squash()` on every element to standardize.
#
# "{ 2026-01-15 -- 2026-01-16, 2026-01-18 -- 2026-01-20, 2026-01-25 -- 2026-01-26 [UTC]}"
# "{ 2026-01-15 -- 2026-01-16, 2026-01-18 -- 2026-01-20 [UTC]}"
#
# Let's leave this for another time -> too much work for V1 and not that important.

# class ------------------------------------------------------------------------

#' @export
new_iset2 <- function(size, starts, ends) {
  stopifnot(is.integer(size))
  stopifnot(is.list(starts))
  stopifnot(is.list(ends))

  vctrs::new_rcrd(
    fields = list(
      size = size,
      starts = starts,
      ends = ends
    ),
    class = "iset2"
  )
}

#' @export
is_iset2 <- function(x) {
  inherits(x, "iset2")
}

#' @export
format.iset2 <- function(x, ...) {
  size <- field(x, "size")

  out <- paste0("{", purrr::map2_chr(
    .x = field(x, "starts"),
    .y = field(x, "ends"),
    .f = \(s, e) paste(paste0("[", s, ", ", e, "]"), collapse = ", ")
  ), "}")

  out[size == 0]   <- "{}"
  out[is.na(size)] <- NA_character_
  out
}

#' @export
is.na.iset2 <- function(x) {
  is.na(field(x, "size"))
}

# set operations ---------------------------------------------------------------

#' @export
intersect_v2 <- function(x, y) {
  stopifnot(is_iset2(x), is_iset2(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  out <- intersect_v2_cpp(
    x_size = field(x, "size"),
    x_starts = field(x, "starts"),
    x_ends = field(x, "ends"),
    y_size = field(y, "size"),
    y_starts = field(y, "starts"),
    y_ends = field(y, "ends")
  )
  vctrs::new_rcrd(fields = out, class = "iset2")
}

#' @export
squash_v2 <- function(x, na.rm = TRUE) {
  stopifnot("`x` must be an <iset2>." = is_iset2(x))

  if (length(x) == 0) {
    return(new_iset2(integer(), list(), list()))
  }
  if (!na.rm && anyNA(field(x, "size"))) {
    return(new_iset2(NA_integer_, list(NA_real_), list(NA_real_)))
  }

  # Missing values (stored as `NULL`) are removed by `unlist()`
  all_starts <- unlist(field(x, "starts"))
  all_ends <- unlist(field(x, "ends"))

  # This is the case where every element in `x` is missing
  if (length(all_starts) == 0) {
    return(new_iset2(NA_integer_, list(NA_real_), list(NA_real_)))
  }

  vctrs::new_rcrd(
    fields = squash_v2_cpp(starts = all_starts, ends = all_ends),
    class = "iset2"
  )
}

#' @export
squash_by <- function(x, by = NULL, na.rm = TRUE) {
  stopifnot("`x` must be an <iset2>." = is_iset2(x))
  stopifnot(obj_is_vector(by), length(by) == 1L || length(by) == length(x))

  if (is.null(by) || length(by) <= 1L) {
    return(squash_v2(x, na.rm))
  }

  groups <- vec_group_loc(by)
  out <- squash_by_cpp(
    size = field(x, "size"),
    starts = field(x, "starts"),
    ends = field(x, "ends"),
    group_locs = groups[["loc"]],
    na_rm = na.rm
  )
  vctrs::new_rcrd(fields = out, class = "iset2")
}
