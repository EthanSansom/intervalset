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
  old_intersect(old_iset1, old_iset2),
  new_intersect(iset1, iset2)
)

# Bench ------------------------------------------------------------------------

# Squash -----------------------------------------------------------------------

squash_starts <- as.numeric(seq(1, 10*1000*5, by = 5))
squash_ends <- squash_starts + 1 + sample(0:15, length(x_starts), TRUE)

# Adding NA's
nas_at <- sample(seq_along(squash_starts), 1000, FALSE)
squash_starts[nas_at] <- NA_real_
squash_ends[nas_at] <- NA_real_
sizes <- rep(1L, length(squash_starts))
sizes[nas_at] <- NA_integer_

new_x <- new_iset(
  sizes = sizes,
  starts = squash_starts,
  ends = squash_ends
)
old_x <- as_old_iset(new_x)

ivs_x <- ivs::iv(squash_starts, squash_ends)

# new_res <- new_squash(new_x)
new_res <- new_squash_2(new_x)
old_res <- old_squash(old_x)

isets_compare(old_res, new_res)

# We're about 2x faster than before, but still slow compared to other methods.
# - `ivs` is 16x faster at "squashing" (AKA identifying groups)
# - TODO: Is there a faster algorithm than sweep-line? How does ivs do it?
#
# UPDATE:
# - Following ivs-ish approach we're much closer with `new_squash_2()`
# - Attempting to remove/sort NA's in C++ was slow, just removing in R
bench::mark(
  new_squash(new_x),
  new_squash_2(new_x),
  old_squash(old_x),
  ivs::iv_groups(ivs_x),
  check = FALSE
)

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
