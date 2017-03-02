

#' Extract coefficients by market from model
#'
#' @param df_with_models
#'
#' @return
#' @export
#'
#' @examples
#' This isnt necessary to run predictions. Instead it provides a way to access
#' the coefficients by market, if one wanted to.
extract_coef_by_mkt <- function(df_with_models){
  df_with_models %>%
    mutate(
      tidy_coef = map(model, .f=create_tidy_coef),
      tidy_flip = map(tidy_coef, .f=create_tidy_flip),
      fixef = map(model, .f=create_fixef),
      out_coef = map2(fixef, tidy_flip, .f=merge)) %>%
    unnest(out_coef)
}
