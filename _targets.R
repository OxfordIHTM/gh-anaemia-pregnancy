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
  )
)


## Output targets
output_targets <- tar_plan(
  
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
