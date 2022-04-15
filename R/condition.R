test_cubble <- function(data) {
  if (!is_cubble(data))
    cli::cli_abort("The function requires the data to be a cubble.
                   Use {.fn as_cubble} with proper {.code key}, {.code index}, and {.code coords}
                   to turn the data into a cubble.")
}

test_long <- function(data){
  test_cubble(data)
  if (form(data) != "long")
    cli::cli_abort("The function requires a long cubble.
                   Use {.fn face_temporal} to turn a cubble into a long one.")
}

test_nested <- function(data){
  test_cubble(data)
  if (form(data) != "nested")
    cli::cli_abort("The function requires a nested cubble.
                   Use {.fn face_spatial} to turn a cubble into a nested one.")
}

test_missing <- function(quo, var){
  if (quo_is_missing(quo)){
    cli::cli_abort("Variable {.code var} is missing for creating a cubble")
  }
}
