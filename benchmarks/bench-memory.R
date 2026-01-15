# preamble ---------------------------------------------------------------------

# NOTE: The flat approach allocates much more memory than the list-of-matrices
#       approach during set operations (e.g. intersection). Looking for the
#       cause.

# setup ------------------------------------------------------------------------

load_all()

set.seed(123)

x_starts <- as.numeric(seq(1, 10*1000*5, by = 5))
x_ends <- x_starts + 1 + sample(0:3, length(x_starts), TRUE)

y_starts <- x_ends + 1 - sample(0:4, length(x_ends), TRUE)
y_ends <- y_starts + 2

# helpers ----------------------------------------------------------------------

# Perform the intersection, but don't create a new vector
intersect_no_initialization <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  intersect_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
}

# Only create the views into `x` and `y`, don't create an output buffer
intersect_span_set_only <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  span_set_only_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
}

# Just extract the inputs
intersect_extract_only <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  out <- list(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
}

# Create an output buffer
intersect_span_buffer_only_cpp <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  span_buffer_only_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
}

# Do nothing
intersect_do_nothing <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  do_nothing_cpp(
    x_size = unattr(unclass(x)),
    x_starts = attr(x, "starts"),
    x_ends = attr(x, "ends"),
    y_size = unattr(unclass(y)),
    y_starts = attr(y, "starts"),
    y_ends = attr(y, "ends")
  )
}

intersect_do_nothing <- function(x, y) {
  stopifnot(is_iset(x), is_iset(y))
  stopifnot(length(x) == length(y) || length(x) == 1L || length(y) == 1L)

  x_size = unattr(unclass(x))
  x_starts = attr(x, "starts")
  x_ends = attr(x, "ends")
  y_size = unattr(unclass(y))
  y_starts = attr(y, "starts")
  y_ends = attr(y, "ends")
}


# benchmarks -------------------------------------------------------------------

# expression           min  median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory
# <bch:expr>       <bch:t> <bch:t>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>
# 1 intersect_v1(x… 548.5µs 574.4µs     1680.   398.8KB     2.49   676     1      402ms <NULL> <Rprofmem>
# 2 intersect_no_in… 526.2µs 562.4µs     1726.   398.8KB     4.15   831     2      481ms <NULL> <Rprofmem>
# 3 intersect_span_…  40.1µs  46.4µs    20345.    78.2KB     6.60  9254     3      455ms <NULL> <Rprofmem>
# 4 intersect_span_…  41.1µs  47.6µs    20414.   273.7KB    20.7   7878     8      386ms <NULL> <Rprofmem>

x <- new_iset(
  sizes = rep(1L, length(x_starts)),
  starts = x_starts,
  ends = x_ends
)
y <- new_iset(
  sizes = rep(1L, length(y_starts)),
  starts = y_starts,
  ends = y_ends
)

# x <- x[1:100]
# y <- y[1:100]

old_x <- as_old_iset(x)
old_y <- as_old_iset(y)

bench::mark(
  # intersect_v0(old_x, old_y),
  intersect_v1(x, y),
  # intersect_no_initialization(x, y),
  # intersect_span_set_only(x, y),
  # intersect_span_buffer_only_cpp(x, y),
  # intersect_extract_only(x, y),
  # intersect_do_nothing_cpp(x, y),
  check = FALSE
)[1:6]

# A tibble: 1 × 6
# expression               min   median `itr/sec` mem_alloc `gc/sec`
# <bch:expr>          <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
# intersect_v1(x, y)    557µs    595µs     1626.     399KB     2.06

x_sizes <- unclass(unattr(x)) # Integer 10,000
x_starts <- attr(x, "starts") # Numeric 10,000
x_ends <- attr(x, "ends")     # Numeric 10,000

lobstr::obj_size(x_sizes)
lobstr::obj_size(x_starts)
lobstr::obj_size(x_ends)

lobstr::obj_size(x_sizes) + lobstr::obj_size(x_starts) + lobstr::obj_size(x_ends)

# 360.90 kB
# - The SpanBuffer allocates 1 set of sizes, starts, ends
# - Shrinking the starts and ends buffers requires creating 1 extra copy of each
( # SpanBuffer
  lobstr::obj_size(x_sizes) +
  lobstr::obj_size(x_starts) +
  lobstr::obj_size(x_ends)
) +
( # SpanBuffer::get_results()
  lobstr::obj_size(x_starts) + lobstr::obj_size(x_ends)
)


