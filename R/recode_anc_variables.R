#'
#' Recode variables
#'

recode_anc_variables <- function(anc_data_processed) {
  anc_data_processed |>
    dplyr::mutate(
      total_n = n(),
      age_group = cut(
        x = age,
        breaks = c(-Inf, 15, 20, 25, 30, 35, 40, 45, Inf),
        labels = c(
          "under 15 years", "15 to 19 years", "20 to 24 years", "25 to 29 years", 
          "30 to 34 years", "35 to 39, years", 
          "40 to 44 years", "45 years and older"
        ),
        include.lowest = TRUE, right = FALSE
      ),
      anaemia_status = ifelse(haemoglobin < 11, "anaemia", "no anaemia"),
      anaemia_category = cut(
        x = haemoglobin,
        breaks = c(0, 7, 10, 11, Inf),
        labels = c("severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia")
      )
    )
}

