# todos ------------------------------------------------------------------------

# TODO: How do NA's act
# - vctrs is going to turn the starts/ends NA element to NULL by default, which
#   I think might actually be okay. When we unlist(), NULL values are removed
#   but in C++ we only check the sizes (not the starts/ends) to verify NA-ness.
#   - We'll have to prevent the SpanBuffer from incrementing, since we no longer
#     store NA values in the un-nested list. This poses a problem for the unchopping
#     since
#   - NOTE: I'm still not sure how this will all interact, I need to think it
#     through.
#
# VERDICT: Use `NULL` for NA values. We won't chop or unlist and instead use
#          lists of doubles as the input/output in C++.

# TODO: Maybe avoid the unlisting?
# - Now that we have a list of starts and list of ends, why not just replace
#   the SpanView with the NumericVector element?

# TODO: See notes, think about using the C level R API to iterate over list elements
#       as fast as possible. I can avoid the unlist if I transform the SpanView
#       into the correct view into the input list-of-doubles for the starts and
#       ends. The key is to avoid allocating any *new* NumericVector's when
#       iterating over the list elements. All I want to do is *see* what the
#       input start and end lists contain, so I shouldn't need to copy ever.
#
#       Look more closely at {vctrs} and {rlang} for how to do this.
#
#       Similarly, you may be able to efficiently allocate and assign into an
#       output list, to avoid the vec_unchop() call.

# TODO: Working with lists
# - map_binary_op:
#   - Input is a list, SpanView sees `VECTOR_ELT(starts, i)`
#   - Output is a list

# class ------------------------------------------------------------------------

#' @export
new_iset2 <- function(size, starts, ends) {
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

  # TODO:
  stop("intersect_v2_cpp() is unimplemented.")

  out <- intersect_v1_cpp(
    x_size = field(x, "size"),
    # TODO: `unlist()` here is faster, but I'm not sure how we interact with NA values yet
    x_starts = field(x, "starts") |> listof_dbl_unchop_cpp(),
    x_ends = field(x, "ends") |> listof_dbl_unchop_cpp(),
    y_size = field(y, "size"),
    y_starts = field(y, "starts") |> listof_dbl_unchop_cpp(),
    y_ends = field(y, "ends") |> listof_dbl_unchop_cpp()
  )

  size <- out[["size"]]
  size_chop <- na_to_one(size)

  new_iset2(
    size,
    out[["starts"]] |> vec_chop(size = size_chop),
    out[["ends"]] |> vec_chop(size = size_chop)
  )
}


#' @export
squash_v2 <- function(x, na.rm = TRUE) {
  stopifnot("`x` must be an <iset2>." = is_iset2(x))

  # TODO:
  stop("squash_v2_cpp() is unimplemented.")

  if (length(x) == 0) {
    return(new_iset2(integer(), list(), list()))
  }
  if (!na.rm && anyNA(field(x, "size"))) {
    return(new_iset2(NA_integer_, list(NA_real_), list(NA_real_)))
  }

  out <- squash_v1_cpp(
    starts = field(x, "starts") |> listof_dbl_unchop_cpp(),
    ends = field(x, "ends") |> listof_dbl_unchop_cpp()
  )

  # squashing is always going to return a scalar, so no need to chop
  new_iset2(out[["size"]], list(out[["starts"]]), list(out[["ends"]]))
}
