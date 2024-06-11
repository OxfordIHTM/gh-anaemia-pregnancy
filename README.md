
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Anaemia in pregnancy in Ghana <a href="https://www.tropicalmedicine.ox.ac.uk/study-with-us/msc-ihtm" target="_blank"><img src="https://podcasts.ox.ac.uk/sites/default/files/image-mirror/unpacking-fundamentals-global-health-towards-new-generation-leadership.png" width="150px" align="right" /></a>

<!-- badges: start -->

[![License for
code](https://img.shields.io/badge/license%20\(for%20code\)-GPL3.0-blue.svg)](https://opensource.org/licenses/gpl-3.0.html)
![License for
data](https://img.shields.io/badge/license%20\(for%20data\)-restricted-red)
<!-- badges: end -->

This repository is a
[`docker`](https://www.docker.com/get-started)-containerised,
[`{targets}`](https://docs.ropensci.org/targets/)-based,
[`{renv}`](https://rstudio.github.io/renv/articles/renv.html)-enabled
[`R`](https://cran.r-project.org/) workflow for the project on *Anaemia
in pregnancy in Ghana*.

## About the Project

## Repository Structure

The project repository is structured as follows:

    gh-anaemia-pregnancy
        |-- .github/
        |-- data/
        |-- data-raw/
        |-- metadata/
        |-- outputs/
        |-- R/
        |-- reports
        |-- renv
        |-- renv.lock
        |-- .Rprofile
        |-- packages.R
        |-- _targets.R

  - `.github` contains project testing and automated deployment of
    outputs workflows via continuous integration and continuous
    deployment (CI/CD) using Github Actions.

  - `data/` contains intermediate and final data outputs produced by the
    workflow.

  - `data-raw/` contains raw datasets, usually either downloaded from
    source or added manually, that are used in the project. This
    directory is empty given that the raw datasets used in this project
    are restricted and are only distributed to eligible members of the
    project. This directory is kept here to maintain reproducibility of
    project directory structure and ensure that the workflow runs as
    expected. Those who are collaborating on this project and who have
    permissions to use the raw datasets should include their copies of
    the raw dataset into this directory in their local versions of this
    repository.

  - `metadata/` contains various metadata for both raw and processed
    datasets found in `data-raw` and `data` directories respectively.

  - `outputs/` contains compiled reports and figures produced by the
    workflow.

  - `R/` contains functions developed/created specifically for use in
    this workflow.

  - `reports/` contains literate code for R Markdown reports rendered in
    the workflow.

  - `renv/` contains `renv` package specific files and directories used
    by the package for maintaining R package dependencies within the
    project. The directory `renv/library`, is a library that contains
    all packages currently used by the project. This directory, and all
    files and sub-directories within it, are all generated and managed
    by the `renv` package. Users should not change/edit these manually.

  - `renv.lock` file is the `renv` lockfile which records enough
    metadata about every package used in this project that it can be
    re-installed on a new machine. This file is generated by the `renv`
    package and should not be changed/edited manually.

  - `.Rprofile` file is a project R profile generated when initiating
    `renv` for the first time. This file is run automatically every time
    R is run within this project, and `renv` uses it to configure the R
    session to use the `renv` project library.

  - `packages.R` file lists out all R package dependencies required by
    the workflow.

  - `_targets.R` file defines the steps in the workflow’s data ingest,
    data processing, data analysis, and reporting pipeline.

## Reproducibility

### R package dependencies

This project was built using `R 4.4.0`. This project uses the `{renv}`
package framework to record R package dependencies and versions.
Packages and versions used are recorded in `renv.lock` and code used to
manage dependencies is in `renv/` and other files in the root project
directory. On starting an R session in the working directory, run
`renv::restore()` to install R package dependencies.

On starting an R session in the working directory, run

``` r
renv::restore()
```

to install this project’s R package dependencies.

### Running the workflow

The current project workflow is described in the image below:

``` mermaid
graph LR
  style Graph fill:#FFFFFF00,stroke:#000000;
  subgraph Graph
    direction LR
    xbba39ca9e518ed94(["anc_data_raw"]):::uptodate --> x597caed207d4fc5d(["anc_data_processed"]):::uptodate
    x597caed207d4fc5d(["anc_data_processed"]):::uptodate --> x34b86f86bb8014ea(["anc_data_processed_metadata"]):::uptodate
    x34b86f86bb8014ea(["anc_data_processed_metadata"]):::uptodate --> x2d4384c638b4d284(["anc_data_processed_metadata_csv"]):::uptodate
    x597caed207d4fc5d(["anc_data_processed"]):::uptodate --> x34a9c3203c3a0d4e(["anc_data_raw_review_report"]):::uptodate
    x34b86f86bb8014ea(["anc_data_processed_metadata"]):::uptodate --> x34a9c3203c3a0d4e(["anc_data_raw_review_report"]):::uptodate
    xbba39ca9e518ed94(["anc_data_raw"]):::uptodate --> x34a9c3203c3a0d4e(["anc_data_raw_review_report"]):::uptodate
    x8a8877168229f4dd(["anc_data_raw_metadata"]):::uptodate --> x34a9c3203c3a0d4e(["anc_data_raw_review_report"]):::uptodate
    x597caed207d4fc5d(["anc_data_processed"]):::uptodate --> x9efa48e5541103b9(["anc_data_processed_csv"]):::uptodate
    x05812169c4dfa932(["anc_data_raw_file"]):::uptodate --> xbba39ca9e518ed94(["anc_data_raw"]):::uptodate
    xbba39ca9e518ed94(["anc_data_raw"]):::uptodate --> x8a8877168229f4dd(["anc_data_raw_metadata"]):::uptodate
    x8a8877168229f4dd(["anc_data_raw_metadata"]):::uptodate --> x2f4ef36220e6f123(["anc_data_raw_metadata_csv"]):::uptodate
  end
```

To run the entire workflow as specified in the current version of the
project, run

``` r
targets::tar_make()
```

from the R console.

You can also run the entire workflow from the command line/Terminal as
follows:

``` bash
Rscript -e "targets::tar_make()"
```

Running specific components of the workflow usually involves specifying
a target name or target names of the components you want to run.
Usually, you should be able to run a full workflow path by just
specifying the name of the last target in the workflow sequence. For
example, the following will run just the *raw data metadata processing
workflow*:

``` r
targets::tar_make(anc_data_raw_metadata_csv)
```

The target `anc_data_raw_metadata_csv` is the last target of a series of
linked metadata processing targets. Hence, to be able to produce the
`anc_data_raw_metadata_csv` target requires running this series of
linked targets.

If you would like to run a set of interrelated but not fully linked
targets, you will likely need to specify more than one target name. For
this, you can use `tidyselect` approaches to name targets to be run. For
example:

``` r
targets::tar_make(dplyr::contains("metadata"))
```

will run all targets whose names contain *“metadata”*.

## Project Team

  - Shih-Ting Tseng - University of Oxford

  - Proochista Ariana - University of Oxford

  - Caesar Atuire - University of Oxford

  - Ernest Guevarra - University of Oxford

## Licenses

All code created through this project (found in this repository) is
released under a [GPL-3.0
license](https://opensource.org/licenses/gpl-3.0.html).

Antenatal care (ANC) data used in this project (not available through
this repository) is restricted to those granted permission to use the
data.

<br> <br>
