load_all()
set.seed(123)

x_iset <- new_iset2(
  size = 1L,
  starts = list(1),
  ends = list(5)
)
y_iset <- new_iset2(
  size = 1L,
  starts = list(2),
  ends = list(10)
)

intersect_v2(x_iset, y_iset)

x <- new_iset2(
  size = c(1L, 2L),
  starts = list(1, c(5, 11)),
  ends = list(2, c(7, 12))
)
x

x[1] <- NA
x
intersect_v2(x, rep(y_iset, 2))

x <- new_iset2(
  size = c(1L, 2L),
  starts = list(1, c(5, 11)),
  ends = list(2, c(7, 12))
)
squash_v2(x)

## Squash
x <- new_iset2(
  size = c(1L, 2L, NA_integer_, 0L),
  starts = list(1, c(5, 11), NULL, numeric()),
  ends = list(2, c(7, 12), NULL, numeric())
)
squash_v2(x, na.rm = FALSE)
squash_by(x, by = c(1, 2, 3, 3), na.rm = FALSE)
x

