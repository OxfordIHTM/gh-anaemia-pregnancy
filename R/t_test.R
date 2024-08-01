#'
#' Perform t-test
#'

test_anc_bivariate_t <- function(anc_data_model_recode) {
  Map(
    f = t.test,
    formula = list(
      eval(parse(text = "haemoglobin ~ early_childbearing")),
      eval(parse(text = "haemoglobin ~ livelihoods")),
      eval(parse(text = "haemoglobin ~ higher_education")),
      eval(parse(text = "haemoglobin ~ marital_status")),
      eval(parse(text = "haemoglobin ~ location"))
    ),
    data = rep(list(anc_data_model_recode), 5)
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
#' Summarise t-test results
#'

summarise_t_test_table <- function(anc_bivariate_t_test) {
  t_test_table <- anc_bivariate_t_test |>
    lapply(
      FUN = function(x) 
        data.frame(
          difference_mean = x$estimate[1] - x$estimate[2],
          lcl = x$conf.int[1],
          ucl = x$conf.int[2],
          p_value = x$p.value
        )
    ) |>
    dplyr::bind_rows(.id = "exposure")
  
  row.names(t_test_table) <- NULL 
  
  t_test_table
}

