
#' Set up coefficients by market
#'
#' @param df_with_models
#'
#' @return
#' @export
#'
#' @examples
setup_coef_by_mkt <- function(df_with_models){
  df_with_models %>%
    mutate(
      tidy_coef = map(model, .f=create_tidy_coef),
      tidy_flip = map(tidy_coef, .f=create_tidy_flip),
      fixef = map(model, .f=create_fixef),
      out_coef = map2(fixef, tidy_flip, .f=merge))
}

