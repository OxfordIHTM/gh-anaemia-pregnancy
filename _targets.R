################################################################################
#
# General Targets Workflow
#
################################################################################

# Load libraries and custom functions -----------------------------------------
suppressPackageStartupMessages(source("packages.R"))
for (f in list.files(here::here("R"), full.names = TRUE)) source (f)

# Create targets and list targets objects -------------------------------------

## Data targets
data_targets <- tar_plan(
  ### Path to raw data ----
  tar_target(
    name = anc_data_raw_file,
    command = "data-raw/Copy of ANC_anon.xlsx",
    format = "file"
  ),
  ### Read raw data ----
  tar_target(
    name = anc_data_raw,
    command = openxlsx::read.xlsx(
      xlsxFile = anc_data_raw_file, 
      sheet = 1,
      detectDates = TRUE
    )
  ),
  ### Create raw data metadata ----
  tar_target(
    name = anc_data_raw_metadata,
    command = create_metadata_raw(anc_data_raw)
  ),
  ### Create raw data metdata CSV ----
  tar_target(
    name = anc_data_raw_metadata_csv,
    command = create_metadata_raw_csv(anc_data_raw_metadata),
    format = "file"
  )
)


## Processing targets
processing_targets <- tar_plan(
  ### Process raw ANC data ----
  tar_target(
    name = anc_data_processed,
    command = process_anc_data_raw(anc_data_raw)
  ),
  ### Create processed ANC data CSV ----
  tar_target(
    name = anc_data_processed_csv,
    command = create_anc_data_processed_csv(anc_data_processed),
    format = "file"
  ),
  ### Create processed ANC data metadata ----
  tar_target(
    name = anc_data_processed_metadata,
    command = create_metadata_processed(anc_data_processed)
  ),
  ### Create processed ANC data metadata CSV ----
  tar_target(
    name = anc_data_processed_metadata_csv,
    command = create_metadata_processed_csv(anc_data_processed_metadata),
    format = "file"
  )
)


## Analysis targets
analysis_targets <- tar_plan(
  ### Recode new ANC data variables ----
  tar_target(
    name = anc_data_recode,
    command = recode_anc_variables(anc_data_processed)
  ),
  ### Create univariate summary tables ----
  tar_target(
    name = anc_data_summary_univariate_table,
    command = summarise_anc_data_univariate(anc_data_recode)
  ),
  ### Create bivariate summary tables ----
  tar_target(
    name = anc_data_summary_bivariate_table,
    command = summarise_anc_data_bivariate(anc_data_recode)
  ),
  ### Recode model variables ----
  tar_target(
    name = anc_data_model_recode,
    command = recode_anc_model_variables(anc_data_recode)
  ),
  ### Create model data ----
  tar_target(
    name = anc_data_model,
    command = create_anc_model_data(anc_data_model_recode)
  ),
  ### Bivariate analysis - odds ratio ----
  tar_target(
    name = anc_bivariate_fisher_test,
    command = test_anc_bivariate_fisher(anc_data_model_recode)
  ),
  ### Summary odds ratio table ----
  tar_target(
    name = anc_odds_ratio_table,
    command = summarise_fisher_test_table(
      anc_bivariate_fisher_test, tidy = TRUE
    )
  ),
  ### Bivariate analysis - t-test ----
  tar_target(
    name = anc_bivariate_t_test,
    command = test_anc_bivariate_t(anc_data_model_recode)
  ),
  ### Summary t-test table ----
  tar_target(
    name = anc_t_test_table,
    command = summarise_t_test_table(anc_bivariate_t_test, tidy = TRUE)
  ),
  ### GLM - logit model ----
  tar_target(
    name = anc_logit_model,
    command = glm(
      formula = anaemia_status ~ early_childbearing + livelihoods + 
        secondary_education + marital_status + location,
      family = binomial(), data = anc_data_model
    )
  ),
  ### GLM - logit model summary table ----
  tar_target(
    name = anc_logit_model_summary,
    command = summarise_glm_output(
      anc_logit_model, exponentiate = TRUE, tidy = TRUE,
      col_names = c("Exposure", "Odds Ratio", "95% CI", "p-value")
    )
  ),
  ### GLM - gaussian model ----
  tar_target(
    name = anc_gaussian_model,
    command = glm(
      formula = haemoglobin ~ early_childbearing + livelihoods + 
        secondary_education + marital_status + location,
      family = gaussian(), data = anc_data_model
    )
  ),
  ### GLM - logit model summary table ----
  tar_target(
    name = anc_gaussian_model_summary,
    command = summarise_glm_output(anc_gaussian_model, tidy = TRUE)
  )
)


## Output targets
output_targets <- tar_plan(
  ### Output recoded ANC data model as CSV ----
  tar_target(
    name = anc_data_model_recode_csv,
    command = create_csv_output(
      df = anc_data_model_recode, path = "data/anc_data_model_recode.csv"
    ),
    format = "file"
  ),
  ### Output ANC data model for GLM ----
  tar_target(
    name = anc_data_model_csv,
    command = create_csv_output(
      df = anc_data_model, path = "data/anc_data_model"
    ),
    format = "file"
  ),
  ### Create XLSX file with all the testing and model results output ----
  tar_target(
    name = anc_model_outputs,
    command = create_xlsx_output(
      anc_odds_ratio_table,
      anc_t_test_table,
      anc_logit_model_summary,
      anc_gaussian_model_summary,
      path = "outputs/anc_model_outputs.xlsx"
    ),
    format = "file"
  )
)


## Reporting targets
report_targets <- tar_plan(
  ### Render data review report ----
  tar_render(
    name = anc_data_raw_review_report,
    path = "reports/gh_anc_data_report.Rmd",
    knit_root_dir = here::here()
  )
)


## Deploy targets
deploy_targets <- tar_plan(
  
)


## List targets
all_targets()
