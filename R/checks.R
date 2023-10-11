check_key_tbl <- function(obj){
  if (!inherits(obj, "key_tbl")){
    cli::cli_abort(
      "The {.field obj} need to be the result from {.fn check_key}.")
  }

}


check_arg_key_use <- function(arg){
  if (!arg %in% c("spatial", "temporal")){
    cli::cli_abort(
      "The {.field key_use} need to be either 'spatial' or 'temporal'.")
  }
}
