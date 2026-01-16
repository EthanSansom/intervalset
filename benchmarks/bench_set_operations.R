# preamble ---------------------------------------------------------------------

# Benchmarks for intersection and squashing. Compares <iset> versions, lubridate,
# and ivs.

# setup ------------------------------------------------------------------------

load_all()
set.seed(123)

library(zeallot)

# data -------------------------------------------------------------------------

## endpoints -------------------------------------------------------------------

sample_spans <- function(n) {
  tibble::lst(
    x_starts = as.numeric(seq(1, n * 5, by = 5)),
    x_ends   = x_starts + 1 + sample(0:3, n, TRUE),
    y_starts = x_ends + 1 - sample(0:4, n, TRUE),
    y_ends   = y_starts + 1 + sample(0:3, n, TRUE)
  )
}

c(x_starts_100, x_ends_100, y_starts_100, y_ends_100) %<-% sample_spans(100)
c(x_starts_10K, x_ends_10K, y_starts_10K, y_ends_10K) %<-% sample_spans(10 * 1000)
c(x_starts_1M, x_ends_1M, y_starts_1M, y_ends_1M)     %<-% sample_spans(1000 * 1000)

## intervals -------------------------------------------------------------------

new_interval <- function(starts, ends, class) {
  if (is.list(starts)) {
    sizes <- lengths(starts)
    sizes[vapply(starts, is.null, TRUE)] <- NA_integer_
  } else {
    sizes <- rep(1L, length(starts))
  }
  switch(
    class,
    iset0 = new_iset0(sizes, starts, ends),
    iset1 = new_iset1(sizes, starts, ends),
    iset2 = new_iset2(sizes, as.list(starts), as.list(ends)),
    iv = ivs::iv(starts, ends),
    Interval = lubridate::interval(as.Date(starts), as.Date(ends))
  )
}

# intervalset
x_iset0_100 <- new_interval(x_starts_100, x_ends_100, "iset0")
y_iset0_100 <- new_interval(y_starts_100, y_ends_100, "iset0")
x_iset0_10K <- new_interval(x_starts_10K, x_ends_10K, "iset0")
y_iset0_10K <- new_interval(y_starts_10K, y_ends_10K, "iset0")
x_iset0_1M  <- new_interval(x_starts_1M, x_ends_1M, "iset0")
y_iset0_1M  <- new_interval(y_starts_1M, y_ends_1M, "iset0")

x_iset1_100 <- new_interval(x_starts_100, x_ends_100, "iset1")
y_iset1_100 <- new_interval(y_starts_100, y_ends_100, "iset1")
x_iset1_10K <- new_interval(x_starts_10K, x_ends_10K, "iset1")
y_iset1_10K <- new_interval(y_starts_10K, y_ends_10K, "iset1")
x_iset1_1M  <- new_interval(x_starts_1M, x_ends_1M, "iset1")
y_iset1_1M  <- new_interval(y_starts_1M, y_ends_1M, "iset1")

x_iset2_100 <- new_interval(x_starts_100, x_ends_100, "iset2")
y_iset2_100 <- new_interval(y_starts_100, y_ends_100, "iset2")
x_iset2_10K <- new_interval(x_starts_10K, x_ends_10K, "iset2")
y_iset2_10K <- new_interval(y_starts_10K, y_ends_10K, "iset2")
x_iset2_1M  <- new_interval(x_starts_1M, x_ends_1M, "iset2")
y_iset2_1M  <- new_interval(y_starts_1M, y_ends_1M, "iset2")

# ivs
x_ivs_100 <- new_interval(x_starts_100, x_ends_100, "iv")
y_ivs_100 <- new_interval(y_starts_100, y_ends_100, "iv")
x_ivs_10K <- new_interval(x_starts_10K, x_ends_10K, "iv")
y_ivs_10K <- new_interval(y_starts_10K, y_ends_10K, "iv")
x_ivs_1M  <- new_interval(x_starts_1M, x_ends_1M, "iv")
y_ivs_1M  <- new_interval(y_starts_1M, y_ends_1M, "iv")

# lubridate
x_lubridate_100 <- new_interval(x_starts_100, x_ends_100, "Interval")
y_lubridate_100 <- new_interval(y_starts_100, y_ends_100, "Interval")
x_lubridate_10K <- new_interval(x_starts_10K, x_ends_10K, "Interval")
y_lubridate_10K <- new_interval(y_starts_10K, y_ends_10K, "Interval")
x_lubridate_1M  <- new_interval(x_starts_1M, x_ends_1M, "Interval")
y_lubridate_1M  <- new_interval(y_starts_1M, y_ends_1M, "Interval")

# benchmark --------------------------------------------------------------------

## intersection ----------------------------------------------------------------

# equivalence test
intersect_v00 <- intersect_v0(x_iset0_100, y_iset0_100)
intersect_v01 <- intersect_v1(x_iset1_100, y_iset1_100)
intersect_v02 <- intersect_v2(x_iset2_100, y_iset2_100)

isets_compare(intersect_v00, intersect_v01)
isets_compare(intersect_v01, intersect_v02)

# intersect 100
bench::mark(
  intersect_v00 = intersect_v0(x_iset0_100, y_iset0_100),
  intersect_v01 = intersect_v1(x_iset1_100, y_iset1_100),
  intersect_v02 = intersect_v2(x_iset2_100, y_iset2_100),
  intersect_lub = lubridate::intersect(x_lubridate_100, y_lubridate_100),
  check = FALSE,
  relative = FALSE
)

# intersect 10K
bench::mark(
  intersect_v00 = intersect_v0(x_iset0_10K, y_iset0_10K),
  intersect_v01 = intersect_v1(x_iset1_10K, y_iset1_10K),
  intersect_v02 = intersect_v2(x_iset2_10K, y_iset2_10K),
  intersect_lub = lubridate::intersect(x_lubridate_10K, y_lubridate_10K),
  check = FALSE,
  relative = FALSE
)

# intersect 1M
bench::mark(
  intersect_v00 = intersect_v0(x_iset0_1M, y_iset0_1M),
  intersect_v01 = intersect_v1(x_iset1_1M, y_iset1_1M),
  intersect_v02 = intersect_v2(x_iset2_1M, y_iset2_1M),
  intersect_lub = lubridate::intersect(x_lubridate_1M, y_lubridate_1M),
  check = FALSE,
  relative = FALSE
)

## squash ----------------------------------------------------------------------

# squash 100
bench::mark(
  squash_v00 = squash_v0(x_iset0_100),
  squash_v01 = squash_v1(x_iset1_100),
  squash_v02 = squash_v2(x_iset2_100),
  squash_ivs = ivs::iv_groups(x_ivs_100),
  check = FALSE,
  relative = FALSE
)

# squash 10K
bench::mark(
  squash_v00 = squash_v0(x_iset0_10K),
  squash_v01 = squash_v1(x_iset1_10K),
  squash_v02 = squash_v2(x_iset2_10K),
  squash_ivs = ivs::iv_groups(x_ivs_10K),
  check = FALSE,
  relative = FALSE
)

# squash 1M
bench::mark(
  squash_v00 = squash_v0(x_iset0_1M),
  squash_v01 = squash_v1(x_iset1_1M),
  squash_v02 = squash_v2(x_iset2_1M),
  squash_ivs = ivs::iv_groups(x_ivs_1M),
  check = FALSE,
  relative = FALSE
)

## slicing ---------------------------------------------------------------------

# [ 100
local({
  i <- seq(100) %% 2 == 0
  bench::mark(
    subset_v00 = x_iset0_100[i],
    subset_v01 = x_iset1_100[i],
    subset_v02 = x_iset2_100[i],
    subset_lub = x_lubridate_100[i],
    check = FALSE,
    relative = FALSE
  )
})

# [ 1M
local({
  i <- seq(1000*1000) %% 2 == 0
  bench::mark(
    subset_v00 = x_iset0_1M[i],
    subset_v01 = x_iset1_1M[i],
    subset_v02 = x_iset2_1M[i],
    subset_lub = x_lubridate_1M[i],
    check = FALSE,
    relative = FALSE
  )
})

# [[ 100
bench::mark(
  subset_v00 = x_iset0_100[[50]],
  subset_v01 = x_iset1_100[[50]],
  subset_v02 = x_iset2_100[[50]],
  subset_lub = x_lubridate_100[[50]],
  check = FALSE,
  relative = FALSE
)

# [[ 1M
bench::mark(
  subset_v00 = x_iset0_1M[[500*1000]],
  subset_v01 = x_iset1_1M[[500*1000]],
  subset_v02 = x_iset2_1M[[500*1000]],
  subset_lub = x_lubridate_1M[[500*1000]],
  check = FALSE,
  relative = FALSE
)

# [[<- 1M
local({
  bench::mark(
    subassign_v00 = x_iset0_1M[[500*1000]] <- NA,
    subassign_v01 = x_iset1_1M[[500*1000]] <- NA,
    subassign_v02 = x_iset2_1M[[500*1000]] <- NA,
    subassign_lub = x_lubridate_1M[[500*1000]] <- NA,
    check = FALSE,
    relative = FALSE
  )
})

# [[<- 1M
local({
  iset0_1 <- x_iset0_100[[1]]
  iset1_1 <- x_iset1_100[[1]]
  iset2_1 <- x_iset2_100[[1]]
  lubridate_1 <- x_lubridate_100[[1]]

  bench::mark(
    subassign_v00 = x_iset0_1M[[500*1000]] <- iset0_1,
    subassign_v01 = x_iset1_1M[[500*1000]] <- iset1_1,
    subassign_v02 = x_iset2_1M[[500*1000]] <- iset2_1,
    subassign_lub = x_lubridate_1M[[500*1000]] <- lubridate_1,
    check = FALSE,
    relative = FALSE
  )
})
