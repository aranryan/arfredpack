
#' Create a panel df
#'
#' @param df
#' @param index
#'
#' @return
#' @export
#'
#' @examples
create_panel_df <- function(df, index) {
  df %>%
    plm::pdata.frame(
      .,
      index = index,
      drop.index = FALSE,
      row.names = TRUE
    )
}
