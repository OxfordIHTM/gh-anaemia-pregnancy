#'
#' Perform Fisher's exact test
#'
#'

test_anc_bivariate_fisher <- function(anc_data_model_recode) {
  Map(
    f = fisher.test,
    x = list(
      with(anc_data_model_recode, table(early_childbearing, anaemia_status)),
      with(anc_data_model_recode, table(livelihoods, anaemia_status)),
      with(anc_data_model_recode, table(higher_education, anaemia_status)),
      with(anc_data_model_recode, table(marital_status, anaemia_status)),
      with(anc_data_model_recode, table(location, anaemia_status))
    ),
    simulate.p.value = TRUE
  ) |>
    (\(x) 
     { 
       names(x) <- c(
         "Early childbearing", "Livelihoods", "Higher education", 
         "Marital status", "Location"
       )
       x 
    }
    )()
}


#'
#' Summarise odds ratio table from fisher test results
#'
#'

summarise_fisher_test_table <- function(anc_bivariate_fisher_test) {
  odds_ratio_table <- anc_bivariate_fisher_test |>
    lapply(
      FUN = function(x) 
        data.frame(
          odds_ratio = x$estimate,
          lcl = x$conf.int[1],
          ucl = x$conf.int[2],
          p_value = x$p.value
        )
    ) |>
    dplyr::bind_rows(.id = "exposure")
  
  row.names(odds_ratio_table) <- NULL 
  
  odds_ratio_table
}