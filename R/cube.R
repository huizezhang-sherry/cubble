# create an S3 class for data cube from tibble/ tsibble to cube
# follow https://vctrs.r-lib.org/articles/s3-vector.html
# learn how a tsibble is built: https://github.com/tidyverts/tsibble/blob/master/R/as-tsibble.R

# need to define time, id, and param

new_cube <- function(x = tibble()){
  vec_assert(x, tibble())
  new_vctr(x, class = "cube")
}

