library(dplyr)
library(openxlsx)
library(gtsummary)
anc_data<-read.csv("data/anc_data_processed06.csv")
#'
#' Summarise glm model output
#' 
#' @param glm_model A glm model object
#' @param tidy Logical. Should summary table to tidied for presentation or for
#'   publication. Default is FALSE.
#'   
#' @returns A summary table with model estimates, lower confidence limit,
#'   upper confidence limit, and p-value. If tidy = TRUE, values are rounded
#'   column names are formalised, and confidence interval column is created
#'   and formatted for presentation or publication purposes.
#'   
#' @examples 
#' summarise_glm_output(anc_logit_model)
#' summarise_glm_output(anc_logit_model, tidy = TRUE)
#' 
#' @export
#'

summarise_glm_output <- function(glm_model, 
                                 exponentiate = FALSE, 
                                 tidy = FALSE,
                                 col_names = c(
                                   "Exposure", "Estimate", "95% CI", "p-value"
                                 )) {
  coef_matrix <- coef(summary(glm_model))
  ci_matrix   <- confint(glm_model)
  
  summary_glm_tab <- data.frame(
    exposure = row.names(coef_matrix[2:nrow(coef_matrix), ]) |>
      stringr::str_replace_all(pattern = "_", replacement = " ") |>
      stringr::str_to_sentence(),
    estimate = coef_matrix[2:nrow(coef_matrix), 1],
    lcl = ci_matrix[2:nrow(ci_matrix), 1],
    ucl = ci_matrix[2:nrow(ci_matrix), 2],
    p_value = coef_matrix[2:nrow(coef_matrix), 4]
  )
  
  row.names(summary_glm_tab) <- NULL
  
  if (exponentiate) {
    summary_glm_tab <- summary_glm_tab |>
      dplyr::mutate(estimate = exp(estimate))
  }
  
  if (tidy) {
    summary_glm_tab <- summary_glm_tab |>
      dplyr::mutate(
        dplyr::across(
          .cols = estimate:ucl,
          .fns = ~scales::label_number(accuracy = 0.01)(.x)
        ),
        p_value = scales::label_pvalue()(p_value),
        ci = paste0(lcl, ", ", ucl)
      ) |>
      dplyr::select(exposure, estimate, ci, p_value) |>
      dplyr::rename_with(
        .fn = function(x) col_names
      )
  }
  
  summary_glm_tab
}


summarise_fisher_test_table <- function(anc_bivariate_fisher_test,
                                        tidy = FALSE) {
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
  
  if (tidy) {
    odds_ratio_table <- odds_ratio_table |>
      dplyr::mutate(
        dplyr::across(
          .cols = odds_ratio:ucl,
          .fns = ~scales::label_number(accuracy = 0.01)(.x)
        ),
        p_value = scales::label_pvalue()(p_value),
        ci = paste0(lcl, ", ", ucl)
      ) |>
      dplyr::select(exposure, odds_ratio, ci, p_value) |>
      dplyr::rename_with(
        .fn = function(x) c("Exposure", "Odds ratio", "95% CI", "p-value")
      )
  }
  
  odds_ratio_table
}

anc_data_recode<-anc_data%>%
  dplyr::mutate(
    anaemia_status=ifelse(haemoglobin<11,"anaemia","no anaemia"),
    early_childbearing = ifelse(age < 20, "Yes", "No") |>
      factor(levels = c("Yes", "No")),
    livelihoods = ifelse(profession %in% c("None", "Student"), "No", "Yes") |>
      factor(levels = c("Yes", "No")),
    secondary_education = ifelse(
      education_level %in% c("None", "Primary"), "No", "Yes"
    ) |>
      factor(levels = c("Yes", "No")),
    location = ifelse(address == "Mfanteman", NA_character_, address),
    location = ifelse(
      address %in% c("Anomabo", "Biriwa", "Asafora"), 
      "Within community", "Outside community"
    ) |>
      factor(levels = c("Within community", "Outside community"))
  )


odds_ratio_table <- Map(
  f = fisher.test,
  x = list(
    with(anc_data_recode, table(early_childbearing, anaemia_status)),
    with(anc_data_recode, table(livelihoods, anaemia_status)),
    with(anc_data_recode, table(secondary_education, anaemia_status)),
    with(anc_data_recode, table(marital_status, anaemia_status)),
    with(anc_data_recode, table(location, anaemia_status))
  ),
  simulate.p.value = TRUE
) |>
  (\(x) 
   { 
     names(x) <- c(
       "Early childbearing", "Livelihoods", "Secondary education", 
       "Marital status", "Location"
     )
     x 
  }
  )() 

summarise_fisher_test_table(odds_ratio_table,tidy=TRUE)


tab_summary <- gtsummary::tbl_summary(
  anc_data_recode |> 
    dplyr::select(
      age, haemoglobin, anaemia_status, early_childbearing, 
      livelihoods, secondary_education, marital_status, location
    ), 
  by = anaemia_status,
  label = list(
    age = "Mean age",
    haemoglobin = "Mean haemoglobin (mg/L)",
    anaemia_status = "Anaemia status",
    early_childbearing = "Early childbearing",
    livelihoods = "Earns a living",
    secondary_education = "At least secondary",
    marital_status = "Marital status",
    location = "Location relative to clinic"
  )
)

tab_summary

tab_summary |>
  gtsummary::as_gt() |>
  gt::gtsave("outputs/descriptive_table.docx")



anaemia_model <- anc_data_recode |>
  dplyr::select(
    haemoglobin, anaemia_status, early_childbearing, 
    livelihoods, secondary_education, marital_status, location
  ) |>
  dplyr::mutate(
    anaemia_status = ifelse(anaemia_status == "anaemia", 1, 0),
    early_childbearing = ifelse(early_childbearing == "Yes", 1, 0),
    livelihoods = ifelse(livelihoods == "Yes", 1, 0),
    secondary_education = ifelse(secondary_education == "Yes", 1, 0),
    marital_status = ifelse(marital_status == "married", 1, 0),
    location = ifelse(location == "Within community", 1, 0)
  ) |>
  na.omit()


logit_model <- glm(
  formula = anaemia_status ~ early_childbearing + livelihoods + 
    secondary_education + marital_status + location, 
  family = binomial(logit), 
  data = anaemia_model
)

summary(logit_model)

summarise_glm_output(logit_model, exponentiate = TRUE, tidy = TRUE)
wb<-createWorkbook()
addWorksheet(wb=wb,sheetName = "odds_table")
addWorksheet(wb=wb,sheetName = "glm_table")
writeData(wb=wb,sheet="odds_table",x=summarise_fisher_test_table(odds_ratio_table,tidy=TRUE))
writeData(wb=wb,sheet="glm_table",x=summarise_glm_output(logit_model, exponentiate = TRUE, tidy = TRUE))
saveWorkbook(wb=wb,file="outputs/model_outputs.xlsx",overwrite = TRUE)