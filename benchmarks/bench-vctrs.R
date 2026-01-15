load_all()

subset <- function(x, i) {
  stopifnot(is_iset(x))
  indices <- vctrs::vec_as_location(i, n = length(x), names = names(x))
  subset_cpp(
    sizes = unattr(unclass(x)),
    starts = attr(x, "starts"),
    ends = attr(x, "ends"),
    indices = indices
  )
}

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

iset1
subset(iset1, c(1, 3))

bench::mark(subset(iset1, c(1, 3)))
bench::mark(vec_proxy(iset1)[c(1, 3)])

# Big --------------------------------------------------------------------------

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
  sizes = rep(split_x_sizes, 200),
  starts = rep(unlist(split_x_starts), 200),
  ends = rep(unlist(split_x_ends), 200)
)
length(attr(new_split_x, "starts")) # 2,000,000
length(new_split_x)                 # 632,600

vec <- seq(length(new_split_x))
lst <- as.list(vec)
i <- vec %% 2 == 0

# Woof. Okay, so sub-setting is 40x slower here.
bench::mark(subset(new_split_x, i))
bench::mark(vec[i])
bench::mark(lst[i])

# Note, this is still brutal!
bench::mark(vec_restore(vec_proxy(new_split_x)[i, ], new_split_x))

bench::mark(subset(new_split_x, 1))
bench::mark(vec[length(vec)])
