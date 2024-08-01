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