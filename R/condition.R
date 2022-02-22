test_cubble <- function(data) {
  if (!is_cubble(data)) cli::cli_abort("data supplied needs to be a cubble object!")
}

test_leaves <- function(data) {
  if (!is_leaves(data)) cli::cli_abort("data supplied needs to be leaves of a cubble!")
}

test_long <- function(data){
  test_cubble(data)
  if (form(data) != "long") cli::cli_abort("data is not in the long form")
}

test_nested <- function(data){
  test_cubble(data)
  if (form(data) != "nested") cli::cli_abort("data is not in the nested form")
}

test_missing <- function(quo, var){
  if (quo_is_missing(quo)){
    cli::cli_abort("Variable {.code var} is missing for creating a cubble")
  }
}
