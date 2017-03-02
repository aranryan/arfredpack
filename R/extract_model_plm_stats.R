
#' Extract plm model characteristics
#'
#' @param df_with_models
#'
#' @return
#' @export
#'
#' @examples
extract_model_plm_stats <- function(df_with_models){
  df_with_models %>%
    mutate(
      glance_eq = map(model, .f=create_glance_eq)) %>%
    unnest(glance_eq) %>%
    # rename certain model characteristics
    rename(
      r_squared = r.squared,
      adj_r_squared = adj.r.squared,
      p_value = p.value,
      df_residual = df.residual,
      p_sig = p.sig) %>%
    # Drop as unnecessary
    select(-train_panel, -model)
}
