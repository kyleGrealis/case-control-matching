# Custom function to format numbers -- was originally adding decimals to
# whole numbers
format_numbers <- function(x) {
  if (all(x == floor(x))) {
    return(format(x, nsmall = 0, scientific = FALSE))
  } else {
    return(x)
  }
}

# custom hover tooltip
box::use(
  bslib[card, tooltip],
)

my_tooltip <- function(...) {
  tooltip(..., "Sort the table by clicking the variable names.", placement = "top")
}
