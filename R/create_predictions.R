


#'Create predictions using make_predictions
#'
#'This takes the coefficients in the working\\_df, joins on the actual data we
#'had set up. Then it calculates the predictions using the make predictions
#'function, and iterating down each row of working\\_df.
#'@param working_df
#'@param df_use
#'@param area_todo
#'
#'@return
#'@export
#'
#' @examples
#'
#'
create_predictions <- function(working_df, df_use, area_todo) {
  working_df %>%
    mutate(
      # Create a data frame by merging the data onto the coefficients
      df_work = map(out_coef, ~left_join(df_use, ., by=c("area_sh"))),
      # Create predictions
      predictions = pmap(.l=list(train_start_date, train_end_date,
                                 test_start_date, test_end_date, df_work, expression),
                         .f=make_predictions, area_todo=area_todo)) %>%
    select(-df_work)
}



