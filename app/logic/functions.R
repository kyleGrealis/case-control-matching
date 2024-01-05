# Custom function to format numbers -- was originally adding decimals to
# whole numbers
format_numbers <- function(x) {
  if (all(x == floor(x))) {
    return(format(x, nsmall = 0, scientific = FALSE))
  } else {
    return(x)
  }
}

