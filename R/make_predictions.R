#' Title
#'
#' @param train_start_date
#' @param train_end_date
#' @param test_start_date
#' @param test_end_date
#' @param df
#' @param expression
#' @param area_todo
#'
#' @return
#' @export
#'
#' @examples
make_predictions <- function(train_start_date, train_end_date,
                             test_start_date, test_end_date, df,
                             expression, area_todo){
output<-NULL
  for(area_sh_i in area_todo) {

    # print(area_sh_i)
    # print(expression)
    # print(train_start_date)
    # print(train_end_date)
    # print("test_start_date")
    # print(test_start_date)
    # print(str(test_start_date))
    # print(as.Date(test_start_date, origin = "1970-01-01"))

    # Filter to a particular area_sh
    df_a <- df %>%
      #filter(area_sh == "phlpa") %>%
      filter_(., .dots= list(~area_sh == area_sh_i)) %>%
      select(-area_sh)

    # print(head(df_a))

    # Set up separate training and test data frames
    train_df <- df_a %>%
      filter(date <= train_end_date)

    test_df <- df_a %>%
      filter(date >= test_start_date & date <= test_end_date)

    # Prepare to do rolling calculations
    Ntrain <- nrow(train_df)
    Ntest <- nrow(test_df)
    testtraindata <- rbind(train_df, test_df)

    # print(head(testtraindata))

    # This is doing the rolling calculation so that it can handle the case with lags.
    # This is a key step in that it is also currently a "by-hand" approach of writing out
    # the equation to calculate the prediction.
    for (i in 1:Ntest) {
      result <- testtraindata[1:Ntrain + i, ] %>%
        mutate(dlogrevparsaupal1 = dplyr::lag(dlogrevparsaupa, 1)) %>%
        mutate_(dlogrevparsaupa = expression) %>%
        # mutate(
        #     dlogrevparsaupa = coef_dloggdptotlcc * dloggdptotlcc +
        #                        coef_dlogsupdsaupa * dlogsupdsaupa
        # ) %>%
        tail(., 1)
      testtraindata[Ntrain + i, "dlogrevparsaupa"] <-
        result[1, "dlogrevparsaupa"]
    }

    df_b <- testtraindata

    train_df <- df_b %>%
      filter(date <= train_end_date)

    predict_df <- df_b %>%
      filter(date >= test_start_date & date <= test_end_date)

    # print(head(predict_df))

    predictions <- grow_dlog2(predict_df, train_df, predict_dlog = "dlogrevparsaupa",
                              predict_level = "revparsaupa")

    # Uses function to extract the prediction levels
    predictions_2 <- arlodr::extract_pred_lev(predictions, "revparsaupa") %>%
      # Add an area_sh column. Not
      mutate(area_sh = area_sh_i) %>%
      # In the following, without the origin part, I was getting an error
      mutate(test_start_date = as.Date(test_start_date, origin = "1970-01-01")) %>%
      select(date, area_sh, test_start_date, everything())

    output <- rbind(output, predictions_2)
    #output <- rbind(output, df_a)
  }
  return(output)
}
