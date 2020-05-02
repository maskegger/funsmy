#' Compute descriptive statistics for the numeric columns of a data frame.
#' @param df The data frame to summarise.
#' @param ... Optional. Columns in the data frame
#' @return A data frame with descriptive statistics. If you are only interested in certain columns
#' you can add these columns.
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' describe(dataset)
#' describe(dataset, col1, col2)
#' }
describe_numeric <- function(df, ...){

  if (nargs() > 1) df <- select(df, ...)

  df %>%
    select_if(is.numeric) %>%
    gather(variable, value) %>%
    group_by(variable) %>%
    summarise_all(list(mean = ~mean(., na.rm = TRUE),
                       sd = ~sd(., na.rm = TRUE),
                       nobs = ~length(.),
                       min = ~min(., na.rm = TRUE),
                       max = ~max(., na.rm = TRUE),
                       q05 = ~quantile(., 0.05, na.rm = TRUE),
                       q25 = ~quantile(., 0.25, na.rm = TRUE),
                       mode = ~as.character(brotools::sample_mode(.), na.rm = TRUE),
                       median = ~quantile(., 0.5, na.rm = TRUE),
                       q75 = ~quantile(., 0.75, na.rm = TRUE),
                       q95 = ~quantile(., 0.95, na.rm = TRUE),
                       n_missing = ~sum(is.na(.)))) %>%
    mutate(type = "Numeric")
}
