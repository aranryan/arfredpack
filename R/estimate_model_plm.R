

#' Estimate model, plm
#'
#'
#' Similar to eviews with "Fixed effects" for the intercept. So this uses
#' separate fixed effects estimate of intercept for each metro
#'
#' The result will be to add
#'
#' @param working_df a dataframe that contains the equations specs, etc. It
#'   needs to have a column with train_end_date that will used to filter the
#'   data as well as a spec column that can be used to generate the equation
#'   specification
#' @param df_use a dataframe with the historical data on the markets. It needs
#'   to have an area_sh column and a data column to be used in creating the
#'   panel dataframe
#' @param model is passed on to the plm::plm function. Should be one of
#'   "pooling", "within", "between", "random", "fd", or "ht". However, I wasn't
#'   able to figure out to get coefficient estimates from the random model.
#'
#' @return Original dataframe, with addition of a train_panel column and a model
#'   column.
#' @export
#'
#' @examples
#'

#'
#'
estimate_model_plm <- function(working_df, df_use, plm_model) {
  temp_1 <- working_df %>%
    mutate(
      # Create a column with the data filtered by training date
      train_df = map(train_end_date, ~filter(df_use, date <= .x)),
      # Create panel dataframe
      train_panel = map(train_df, ~create_panel_df(.x, index =c("area_sh", "date"))),
      # Define spec
      spec_f = map(spec, .f=as.formula),
      # Estimate the model
      model = map2(.x=spec_f, .y=train_panel, .f=plm::plm, model = plm_model)) %>%
    # Drop as unnecessary
    select(-train_df, -spec_f)
}
