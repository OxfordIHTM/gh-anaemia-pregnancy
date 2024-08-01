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
  output_tables <- list(
    "odds_ratio" = anc_odds_ratio_table,
    "t_test" = anc_t_test_table,
    "logit_model" = anc_logit_model_summary,
    "gaussian_model" = anc_gaussian_model_summary
  )
  
  wb <- openxlsx::buildWorkbook(
    x = output_tables,
    colWidths = "auto"
  )
  
  openxlsx::saveWorkbook(wb = wb, file = path, overwrite = TRUE)
  
  path
}
