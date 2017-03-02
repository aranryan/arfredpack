#' Extract some info about the equations
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
create_glance_eq <- function(model) {
  glance_eq <- broom::glance(model) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(
                            p.value < 0.01,  "**",
                            ifelse(p.value < 0.1, "*",
                                   ifelse(p.value < 0.2, "-", ""))
                          )))
}
