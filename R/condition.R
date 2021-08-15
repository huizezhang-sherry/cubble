test_cubble <- function(data) {
  if (!is_cubble(data)) abort("data supplied needs to be a cubble object!")
}

test_leaves <- function(data) {
  if (!is_leaves(data)) abort("data supplied needs to be leaves of a cubble!")
}

test_long <- function(data){
  test_cubble(data)
  if (form(data) != "long") abort("data is not in the long form")
}

test_nested <- function(data){
  test_cubble(data)
  if (form(data) != "nested") abort("data is not in the nested form")
}
