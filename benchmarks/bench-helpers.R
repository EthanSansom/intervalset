load_all()

shuffle <- function(x) sample(x, length(x), FALSE)

# Chopping ---------------------------------------------------------------------

# Copy of potential function to prevent NA as NULL problem
points_unchop <- function(x) {
  null_at <- vapply(x, is.null, TRUE)
  x[null_at] <- list(NA_real_)
  unlist(x)
}

n <- 10*1000*1000
points <- shuffle(as.numeric(1:n))
groups <- sample(1:(5*1000*1000), n, TRUE)
chopped <- unname(split(points, groups))

chopped_with_na <- chopped
chopped_with_na[sample(seq_along(chopped_with_na), 1000, FALSE)] <- list(NULL)

# Large (10 million, 5M groups) (actually 4,323,260 groups)
# - unlist() is much faster
# - we only introduce NULL when performing sub-assignment with NA values
#   - so, sanitize using the `[[<-` and `[<-` methods (possibly `c()`)
bench::mark(
  "unlist" = unlist(chopped),
  "10M/5M no NA" = points_unchop(chopped),
  "10M/5M w/ NA" = points_unchop(chopped_with_na),
  check = FALSE
)

# Large (10 million, 5M groups)
# - Median 411ms, so we'll definitely notice a slow-down at larger sizes
#   - But these are huge numbers, wouldn't work with lubridate objects that large either
sizes <- lengths(chopped)
bench::mark(
  "10M/5M" = vec_chop(points, sizes = sizes),
  check = FALSE
)

# Indexing ---------------------------------------------------------------------

## Generic
x <- 1:5

x[0]           # Every element is NA
x[NA]          # Coerce every element to NA
x[NA] <- 0; x  # Does nothing

x[c(NA, NA)]                    # Coerce every element to NA
x[c(TRUE, NA)]                  # Coerce every second element to NA
x[c(TRUE, NA, FALSE, TRUE, NA)] # Get first, forth elements. Two NA's in between

x[[NA]]       # Error
x[[0]]        # Error

vctrs::vec_slice(x, NA)         # Coerce to NA
vctrs::vec_slice(x, c(NA, NA))  # Error
vctrs::vec_slice(x, logical(0)) # Error

vctrs::vec_slice(x, integer(0)) # No Elements

## Vctrs
iset <- new_iset(
  sizes = c(1L, 1L, 2L),
  starts = as.numeric(1:4),
  ends = as.numeric(2:5)
)
iset

# FIXED
iset[NA]
iset[c(1, NA)]
iset[1]
unclass(iset[1])

# iset[TRUE] <- 10
unclass(iset[[TRUE]]) # TODO: This causes an error during restoration!

# TODO: Sub-assignment needs to use the proxy!
# UGH: Sub-assignment is still bad
copy <- iset
copy[1] <- NA
unclass(copy)

copy <- iset
copy[TRUE] <- NA
unclass(copy)

copy <- iset
vctrs::vec_assign(copy, 1, NA)
unclass(copy)

# Also bad!
copy <- iset
copy[[1]] <- NA
unclass(copy)

# `c()` WORKS!
c(iset, NA, iset)
c(iset, NA, iset) |> unclass()

# YAY: This is actually correct
unclass(vec_cast(NA, iset))

unclass(vec_cast(NA, iset)) |> class()        # Integer
attr(vec_cast(NA, iset), "starts") |> class() # Numeric
attr(vec_cast(NA, iset), "ends") |> class()   # Numeric

# YAY: Also correct!
iset[NULL] |> unclass()

# We use the `[.vctrs_vctr` method which calls vec_slice() on the proxy

# Sets `sizes` to NA
# 1. When assigning NA to the proxy here `starts` and `ends` are lists, so lst[NA] is a `NULL`
# 2. When we restore, the `unlist()` call creates `NULL` (empty) `starts` and `ends`
# 3. So `vec_proxy()` returns just the `sizes` column, an integer
unclass(iset[NA])

# These have the same problem as above ^
unclass(iset[c(NA, TRUE, FALSE)])
unclass(iset[c(1, NA)])

# vec_slice() prevents this (incorrect size)
try(iset[c(NA, TRUE)])

# Also bad, we assign an NA value to `sizes`, but do nothing the the `ends`/`starts`
copy <- iset
copy[1] <- NA
unclass(copy)

iset[[NA]] # Uses `[[.default`, throws an error

# Also bad, so we need to intervene in `NA` assignment and probably `c()`
copy <- iset
copy[[2]] <- NA
unclass(copy)

iset[NA]          # Un-classes, coerces `sizes` to NA
iset[c(NA, TRUE)] # Same behavior as `[<-` for numeric, because we're calling next method

vctrs::vec_slice(x, c(NA, NA))

sloop::s3_dispatch(iset[c(NA, TRUE)])

## What about list_of()?
a <- as_list_of(list(1, 2, 3), "numeric")
a

a[NA] # Shoot, still coerces elements to `NULL`

# CPP Version ------------------------------------------------------------------

test_points <- list(NULL, as.numeric(1:5), as.numeric(10:12), numeric(0), 10)
sizes <- lengths(test_points)
sizes[vapply(test_points, is.null, TRUE)] <- NA_integer_

points_unchop_cpp_refined(test_points)
points_unchop_cpp(test_points, sizes)
points_unchop(test_points)

bench::mark(
  refined = points_unchop_cpp_refined(test_points),
  fast = list_of_numeric_unchop_cpp(test_points),
  default = points_unchop_cpp(test_points, sizes),
  r = points_unchop(test_points)
)

chopped_with_na_sizes <- lengths(chopped_with_na)
chopped_with_na_sizes[vapply(chopped_with_na, is.null, TRUE)] <- NA_integer_

bench::mark(
  refined = points_unchop_cpp_refined(chopped_with_na),
  fast = list_of_numeric_unchop_cpp(chopped_with_na),
  default = points_unchop_cpp(chopped_with_na, chopped_with_na_sizes),
  r = points_unchop(chopped_with_na)
)
