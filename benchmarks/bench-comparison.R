# setup ------------------------------------------------------------------------

load_all()

shuffle <- function(x) sample(x, length(x), FALSE)

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
  intersect_v0(old_iset1, old_iset2),
  intersect_v1(iset1, iset2)
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

new2_x <- as_new2_iset(new_x)
new2_y <- as_new2_iset(new_y)

new_res <- intersect_v1(new_x, new_y)
old_res <- intersect_v0(old_x, old_y)

isets_compare(old_res, new_res)
isets_compare(new_res, intersect_v1(new2_x, new2_y))

lub_x <- lubridate::interval(as.Date(attr(new_x, "starts")), as.Date(attr(new_x, "ends")))
lub_y <- lubridate::interval(as.Date(attr(new_y, "starts")), as.Date(attr(new_y, "ends")))

bench::mark(
  old = intersect_v0(old_x, old_y),
  new = intersect_v1(new_x, new_y),
  lst = intersect_v1(new2_x, new2_y),
  lub = lubridate::intersect(lub_x, lub_y),
  check = FALSE,
  relative = FALSE
)

# BIG!

big_old_x <- rep(old_x, 100)
big_old_y <- rep(old_y, 100)
big_new_x <- as_new_iset(big_old_x)
big_new_y <- as_new_iset(big_old_y)
big_new2_x <- as_new2_iset(big_old_x)
big_new2_y <- as_new2_iset(big_old_y)
big_lub_x <- lubridate::interval(as.Date(attr(big_new_x, "starts")), as.Date(attr(big_new_x, "ends")))
big_lub_y <- lubridate::interval(as.Date(attr(big_new_y, "starts")), as.Date(attr(big_new_y, "ends")))

bench::mark(
  old = intersect_v0(big_old_x, big_old_y),
  new = intersect_v1(big_new_x, big_new_y),
  new2 = intersect_v1(big_new2_x, big_new2_y),
  lub = lubridate::intersect(big_lub_x, big_lub_y),
  check = FALSE,
  relative = FALSE
)

# LIST OF
# - Access is *so* slow. I wonder what the slowdown for one unlist is?
x_sizes <- unattr(unclass(new_x))
x_sizes_chop <- x_sizes
x_sizes_chop[is.na(x_sizes_chop)] <- 1L

x_starts_lst <- vctrs::vec_chop(attr(new_x, "starts"), sizes = x_sizes_chop)
x_ends_lst   <- vctrs::vec_chop(attr(new_x, "ends"), sizes = x_sizes_chop)

y_sizes <- unattr(unclass(new_y))
y_sizes_chop <- y_sizes
y_sizes_chop[is.na(y_sizes_chop)] <- 1L

y_starts_lst <- vctrs::vec_chop(attr(new_y, "starts"), sizes = y_sizes_chop)
y_ends_lst   <- vctrs::vec_chop(attr(new_y, "ends"), sizes = y_sizes_chop)

bench::mark(
  old = intersect_v0(old_x, old_y),
  new = intersect_v1(new_x, new_y),
  lub = lubridate::intersect(lub_x, lub_y),
  lst = new_intersect_list_test(x_sizes, x_starts_lst, x_ends_lst, y_sizes, y_starts_lst, y_ends_lst),
  check = FALSE,
  relative = FALSE
)[1:6]
length(x_sizes)

res1 <- new_intersect_list_test(x_sizes, x_starts_lst, x_ends_lst, y_sizes, y_starts_lst, y_ends_lst)
res2 <- intersect_v1(new_x, new_y)
identical(res1, res2)

## BIG LIST OF
x_sizes <- unattr(unclass(big_new_x))
x_sizes_chop <- x_sizes
x_sizes_chop[is.na(x_sizes_chop)] <- 1L

x_starts_lst <- vctrs::vec_chop(attr(big_new_x, "starts"), sizes = x_sizes_chop)
x_ends_lst   <- vctrs::vec_chop(attr(big_new_x, "ends"), sizes = x_sizes_chop)

y_sizes <- unattr(unclass(big_new_y))
y_sizes_chop <- y_sizes
y_sizes_chop[is.na(y_sizes_chop)] <- 1L

y_starts_lst <- vctrs::vec_chop(attr(big_new_y, "starts"), sizes = y_sizes_chop)
y_ends_lst   <- vctrs::vec_chop(attr(big_new_y, "ends"), sizes = y_sizes_chop)

big_lub_x <- lubridate::interval(as.Date(attr(big_new_x, "starts")), as.Date(attr(big_new_x, "ends")))
big_lub_y <- lubridate::interval(as.Date(attr(big_new_y, "starts")), as.Date(attr(big_new_y, "ends")))

bench::mark(
  old = intersect_v0(big_old_x, big_old_y),
  new = intersect_v1(big_new_x, big_new_y),
  lub = lubridate::intersect(big_lub_x, big_lub_y),
  lst = new_intersect_list_test(x_sizes, x_starts_lst, x_ends_lst, y_sizes, y_starts_lst, y_ends_lst),
  check = FALSE,
  relative = FALSE
)[1:6]
length(x_sizes)

big_lst_x <- vctrs::new_rcrd(
  fields = list(
    sizes = x_sizes,
    starts = x_starts_lst,
    ends = x_ends_lst
  ),
  class = "rcrd_iset"
)

i <- seq_along(x_sizes) %% 2 == 0
bench::mark(
  big_lst_x[i],
  big_new_x[i],
  check = FALSE
)

res1 <- intersect_v1(big_new_x, big_new_y)
res2 <- new_intersect_list_test(x_sizes, x_starts_lst, x_ends_lst, y_sizes, y_starts_lst, y_ends_lst)

res2 <- new_iset(
  sizes = res2$size,
  starts = unlist(res2[[2]]),
  ends = unlist(res2[[3]])
)
identical(res1, res2)

# TODO: We need to speed up access!
# - Circumvent the proxy and manually create the new iset using the `sizes` to
#   get the correct offsets
bench::mark(
  big_old_x[[100]],
  big_new_x[[100]],
  check = FALSE
)


## Squash ----------------------------------------------------------------------

squash_starts <- as.numeric(seq(1, 10*1000*5, by = 5))
squash_ends <- squash_starts + 1 + sample(0:15, length(x_starts), TRUE)
sizes <- rep(1L, length(squash_starts))

# Adding NA's
# nas_at <- sample(seq_along(squash_starts), 1000, FALSE)
# squash_starts[nas_at] <- NA_real_
# squash_ends[nas_at] <- NA_real_
# sizes[nas_at] <- NA_integer_

new_x <- new_iset(
  sizes = sizes,
  starts = squash_starts,
  ends = squash_ends
)
old_x <- as_old_iset(new_x)

ivs_x <- ivs::iv(squash_starts, squash_ends)

new_res <- squash_v1(new_x)
old_res <- squash_v0(old_x)

isets_compare(old_res, new_res)

# We're about 2x faster than before, but still slow compared to other methods.
# - `ivs` is 16x faster at "squashing" (AKA identifying groups)
# - TODO: Is there a faster algorithm than sweep-line? How does ivs do it?
#
# UPDATE:
# - Following ivs-ish approach we're much closer with `new_squash_2()`
# - Attempting to remove/sort NA's in C++ was slow, just removing in R
bench::mark(
  squash_v1(new_x),
  squash_v0(old_x),
  ivs::iv_groups(ivs_x),
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

new_res <- intersect_v1(new_split_x, new_split_y)
old_res <- intersect_v0(old_split_x, old_split_y)

isets_compare(old_res, new_res)

bench::mark(
  old = intersect_v0(old_split_x, old_split_y),
  new = intersect_v1(new_split_x, new_split_y),
  check = FALSE,
  relative = TRUE
)

bench::mark(
  old = intersect_v0(old_split_x[[1]], old_split_y),
  new = intersect_v1(new_split_x[[1]], new_split_y),
  check = FALSE,
  relative = TRUE
)
