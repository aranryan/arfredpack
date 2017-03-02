
#' Extract the fixed effects for each market
#'
#' @param model
#'
#' @return
#' @export
#'
#' @examples
#'
#'The fixef function extracts the fixed effects from a plm object
#' these are the intercepts specific to each market, such as
#' when the plm model is estimated with model="within"
#' not sure what it would do if the fixed effects aren't there.
#' By setting type = level, we are getting the level of the intercept,
#' other choices would be in terms of deviation from the mean across markets.
#'
create_fixef <- function(model) {
  fixef <- plm::fixef(model, type = 'level')
  a <- data.frame(fixef[1:length(fixef)])
  a$area_sh <- rownames(a)
  row.names(a) <- NULL
  colnames(a) <- c("intercept", "area_sh")
  fixef <- a %>%
    select(area_sh, intercept)
}
