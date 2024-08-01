#'
#' Create generic CSV output
#' 
#' @param df A data.frame object to output into CSV.
#' @param path Path to output CSV file.
#' 
#' @returns Path to output CSV file.
#' 
#' @examples
#' create_csv_output(anc_data_model, "data/anc_data_model.csv")
#'
#' @export
#'

create_csv_output <- function(df, path) {
  write.csv(x = df, file = path, row.names = FALSE)
  
  path
}


#'
#' Create statistical test and model results outputs
#'
#'

create_xlsx_output <- function(anc_odds_ratio_table,
                               anc_t_test_table,
                               anc_logit_model_summary,
                               anc_gaussian_model_summary,
                               path) {
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "odds_ratio")
  openxlsx::writeData(wb, "odds_ratio", anc_odds_ratio_table)
  
  openxlsx::addWorksheet(wb, "t_test")
  openxlsx::writeData(wb, "t_test", anc_t_test_table)
  
  openxlsx::addWorksheet(wb, "logit_model")
  openxlsx::writeData(wb, "logit_model", anc_logit_model_summary)
  
  openxlsx::addWorksheet(wb, "gaussian_model")
  openxlsx::writeData(wb, "gaussian_model", anc_gaussian_model_summary)
  
  openxlsx::saveWorkbook(wb = wb, file = path, overwrite = TRUE)
  
  path
}
