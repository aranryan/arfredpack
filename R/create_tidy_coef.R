



#' Extract coefficients
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
create_tidy_coef <- function(model) {
  tidy_coef <- broom::tidy(model) %>%
    mutate(p.sig = ifelse(p.value < 0.001, "***",
                          ifelse(
                            p.value < 0.01,  "**",
                            ifelse(p.value < 0.1, "*",
                                   ifelse(p.value < 0.2, "-", ""))
                          ))) %>%
    select(everything())
}
