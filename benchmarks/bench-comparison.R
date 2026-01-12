# setup ------------------------------------------------------------------------

load_all()

shuffle <- function(x) sample(x, length(x), FALSE)

# helpers ----------------------------------------------------------------------

isets_compare <- function(old_iset, new_iset) {
  stopifnot(inherits(old_iset, "iset0"), inherits(new_iset, "iset"))
  old_sizes <- purrr::map_int(old_iset, ~ ifelse(is.null(.x), NA_integer_, nrow(.x)))
  old_starts <- purrr::map(old_iset, ~ if (is.null(.x)) { NA_real_ } else { .x[, 1] }) |> unlist()
  old_ends <- purrr::map(old_iset, ~ if (is.null(.x)) { NA_real_ } else { .x[, 2] }) |> unlist()

  new_sizes <- unattr(unclass(new_iset))
  new_starts <- attr(new_iset, "starts")
  new_ends <- attr(new_iset, "ends")

  waldo::compare(
    list(sizes = old_sizes, starts = old_starts, ends = old_ends),
    list(sizes = new_sizes, starts = new_starts, ends = new_ends)
  )
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

# testing ----------------------------------------------------------------------

iset1 <- new_iset(
  sizes = c(NA, 1, 2),
  starts = c(NA, 1, 2, 5),
  ends = c(NA, 2, 3, 6)
)

iset2 <- new_iset(
  sizes = c(1),
  starts = c(4),
  ends = c(7)
)

old_iset1 <- new_iset0(
  sizes = c(NA, 1, 2),
  starts = c(NA, 1, 2, 5),
  ends = c(NA, 2, 3, 6)
)
old_iset2 <- new_iset0(
  sizes = c(1),
  starts = c(4),
  ends = c(7)
)

isets_compare(
  old_intersect(old_iset1, old_iset2),
  new_intersect(iset1, iset2)
)

# Bench ------------------------------------------------------------------------

## Single Spans ----------------------------------------------------------------

# These are intervals of 1 to 4 in length
x_starts <- as.numeric(seq(1, 10*1000*5, by = 5))
x_ends <- x_starts + 1 + sample(0:3, length(x_starts), TRUE)

# These are intervals which potentially intersect the x intervals
y_starts <- x_ends + 1 - sample(0:4, length(x_ends), TRUE)
y_ends <- y_starts + 2

new_x <- new_iset(
  sizes = rep(1, length(x_starts)),
  starts = x_starts,
  ends = x_ends
)
new_y <- new_iset(
  sizes = rep(1, length(y_starts)),
  starts = y_starts,
  ends = y_ends
)
old_x <- as_old_iset(new_x)
old_y <- as_old_iset(new_y)

new_res <- new_intersect(new_x, new_y)
old_res <- old_intersect(old_x, old_y)

isets_compare(old_res, new_res)

bench::mark(
  old = old_intersect(old_x, old_y),
  new = new_intersect(new_x, new_y),
  check = FALSE,
  relative = TRUE
)

big_old_x <- rep(old_x, 100)
big_old_y <- rep(old_y, 100)
big_new_x <- as_new_iset(big_old_x)
big_new_y <- as_new_iset(big_old_y)

bench::mark(
  old = old_intersect(big_old_x, big_old_y),
  new = new_intersect(big_new_x, big_new_y),
  check = FALSE,
  relative = TRUE
)

# TODO: We need to speed up access!
# - Circumvent the proxy and manually create the new iset using the `sizes` to
#   get the correct offsets
bench::mark(
  big_old_x[[100]],
  big_new_x[[100]],
  check = FALSE
)

### Access ---------------------------------------------------------------------

bench::mark(
  old_x[[200]],
  new_x[[200]],
  check = FALSE
)

bench::mark(
  old_x[seq(length(old_x)) %% 2 == 0],
  new_x[seq(length(new_x)) %% 2 == 0],
  check = FALSE
)

## Multiple Spans --------------------------------------------------------------

set.seed(123)

# These are intervals of 1 to 4 in length
x_starts <- as.numeric(seq(1, 10*1000*5, by = 5))
x_ends <- x_starts + 1 + sample(0:3, length(x_starts), TRUE)

# These are intervals which potentially intersect the x intervals
y_starts <- x_ends + 1 - sample(0:4, length(x_ends), TRUE)
y_ends <- y_starts + 2

groups <- sample(1:(10*1000 %/% 3), length(x_starts), TRUE)

split_x_starts <- x_starts |> split(groups)
split_x_ends <- x_ends |> split(groups)
split_y_starts <- y_starts |> split(groups)
split_y_ends <- y_ends |> split(groups)

split_x_sizes <- lengths(split_x_starts)
split_y_sizes <- lengths(split_y_starts)

new_split_x <- new_iset(
  sizes = split_x_sizes,
  starts = unlist(split_x_starts),
  ends = unlist(split_x_ends)
)
new_split_y <- new_iset(
  sizes = split_y_sizes,
  starts = unlist(split_y_starts),
  ends = unlist(split_y_ends)
)
old_split_x <- as_old_iset(new_split_x)
old_split_y <- as_old_iset(new_split_y)

new_res <- new_intersect(new_split_x, new_split_y)
old_res <- old_intersect(old_split_x, old_split_y)

isets_compare(old_res, new_res)

bench::mark(
  old = old_intersect(old_split_x, old_split_y),
  new = new_intersect(new_split_x, new_split_y),
  check = FALSE,
  relative = TRUE
)

bench::mark(
  old = old_intersect(old_split_x[[1]], old_split_y),
  new = new_intersect(new_split_x[[1]], new_split_y),
  check = FALSE,
  relative = TRUE
)
