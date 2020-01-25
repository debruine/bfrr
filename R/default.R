#' Give default value if doesn't exist
#'
#' @param x The value to test if it exists
#' @param default The default value
#' @param rm_na Replace NA values with the default
#'
#' @return the value or default
#' @export
#'
#' @examples
#' a <- 1
#' default(a, 2) # should return 1
#' remove(a)
#' default(a, 2) # should return 2
#'
default <- function(x, default, rm_na = TRUE) {
  mytry <- try(x, TRUE)
  # object not found
  if (class(mytry) == 'try-error') return(default)

  # handles var$missing
  if (is.null(mytry)) return(default)

  if (is.list(mytry)) {
    nm <- names(mytry)
    if (is.null(nm)) {
      # unnamed list, replace NAs
      if (rm_na) x[is.na(x)] <- default
      return(x)
    }
    # handles var["missing"]
    if (is.na(nm)) return(default)
  } else { # not a list
    # replace any NAs with the default

    if (rm_na & sum(is.na(x)) > 0) {
      if (length(default) == length(x)) {
        # replace with equivalent position in default
        x[is.na(x)] <- default[is.na(x)]
      } else {
        x[is.na(x)] <- default
      }
    }
  }
  return(x)
}
