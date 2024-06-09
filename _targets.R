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
  
)


## Analysis targets
analysis_targets <- tar_plan(
  
)


## Output targets
output_targets <- tar_plan(
  
)


## Reporting targets
report_targets <- tar_plan(
  
)


## Deploy targets
deploy_targets <- tar_plan(
  
)


## List targets
all_targets()
