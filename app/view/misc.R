#' This script is for miscellaneous, reusable components

box::use(
  bslib[tooltip],
)
#' @export
my_tooltip <- function(x) {
  tooltip(x, "Sort the table by clicking the variable names.", placement = "top")
}
