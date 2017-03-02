# These are functions I created while doing the plm (pooled) modeling for Host.
# I wanted them in a package so that I could use some of them to generate
# predictions from saved models in another script.

#' Turn the right hand side variables into an expression.
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
make_expression <- function(x) {

  a <- x
  a

  b <- strsplit(a, split=" ")[[1]]
  b

  c <- grep("\\+", b, invert=TRUE, value=TRUE)
  c

  # Replace the way the lag term is written
  d <- gsub("lag\\(dlogrevparsaupa\\)", "dlogrevparsaupal1", c)
  d

  e <- paste(" coef_", d, sep="")
  e
  f <- paste(e, "* ")
  f

  # create another version of the string with pluses
  d_2 <- paste(d, "+")

  g <- c(matrix(c(f, d_2), 2, byrow = T))
  g
  h <- paste(g, sep="", collapse="") %>%
    # trim the white space
    trimws()
  h
  # delete the last three characters of the string, as these
  # contain a " +"
  i <- stringi::stri_sub(h, 1, -3) %>%
    trimws()
  i
}
