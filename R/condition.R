test_cubble <- function(data) {
  if (!is_cubble(data)) abort(glue::glue("{data} needs to be a cubble object!"))
}
