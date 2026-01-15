isets_compare <- function(x, y) {
  waldo::compare(as_iset1(x), as_iset1(x))
}

as_iset1 <- function(x) {
  switch(
    class(x)[[1]],
    iset0 = iset0_to_iset1(x),
    iset1 = x,
    iset2 = iset2_to_iset1(x),
    default = stop("Unimplemented")
  )
}

iset0_to_iset1 <- function(x) {
  stopifnot(is_iset0(x))
  sizes <- purrr::map_int(x, ~ ifelse(is.null(.x), NA_integer_, nrow(.x)))
  starts <- purrr::map(x, ~ if (is.null(.x)) { NA_real_ } else { .x[, 1] }) |> unlist()
  ends <- purrr::map(x, ~ if (is.null(.x)) { NA_real_ } else { .x[, 2] }) |> unlist()
  new_iset1(sizes, starts, ends)
}

iset2_to_iset1 <- function(x) {
  stopifnot(is_iset2(x))
  new_iset(
    sizes = field(x, "sizes"),
    starts = purrr::modify_if(field(x, "starts"), is.null, ~ NA_real_) |> unlist(),
    ends = purrr::modify_if(field(x, "ends"), is.null, ~ NA_real_) |> unlist()
  )
}
