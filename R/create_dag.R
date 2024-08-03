#'
#' Create DAG for maternal anaemia
#'
#' @details
#'   MA  - outcome; maternal anaemia
#'   ND  - nutrient deficiencies
#'   IDI - inadequate dietary intake
#'   NM  - nutrient malabsorption
#'   NL  - nutrient loss
#'   LIS - low iron stores
#'   IF  - infection
#'   IM  - inflammation
#'   CD  - chronic disease
#'   PH  - physiologic factors
#'   RBD - red blood cell disorders
#'   IDP - inappropriate dietary practices
#'   NFR - lack of access to nutrient-rich foods
#'   LHR - limited household resources
#'   LAP - lack of awareness of appropriate dietary practices
#'   LED - lack of formal education
#'   LL  - lack of livelihoods
#'
#'

create_maternal_anaemia_dag <- function() {
  dagitty::dagitty(
    "dag {
      IDI -> ND -> MA
      NM  -> ND
      NL  -> ND
      IM  -> NM
      IF  -> IM
      IF  -> NM
      IF  -> NL
      CD  -> IM
      PH  -> NL
      LIS -> ND
      RBD -> LIS
      
      LAP -> IDP -> IDI
      LED -> IDP
      LHR -> LED
      
      NFR -> IDI
      LHR -> NFR
      LL  -> LHR
      LED -> LL
      LED -> LAP
      
      MA [outcome]
    }"
  ) |>
    ggdag::ggdag()
}


#'
#' Create a directed acyclic graph (DAG) of the study variables
#' 
#' @param tidy Logical. Should the DAG be tidied? Default is FALSE.
#' @param lables Logical. Should labels 
#'


create_study_dag <- function(tidy = FALSE, labels = FALSE) {
  study_dag <- dagitty::dagitty(
    "dag {
      AGE     -> MA
      LH      -> MA
      EDU     -> MA
      MARRIED -> MA
      LOC     -> MA

      AGE     -> MARRIED
      AGE     -> LH
      EDU     -> LH

      MA [outcome]
    }"
  )
  
  if (tidy) {
    study_dag <- ggdag::tidy_dagitty(study_dag)
  }
  
  study_dag
}

#'
#' Plot the directed acyclic graph (DAG) of the study
#'

plot_study_dag <- function(study_dag) {
  ggdag::ggdag(
    .tdy_dag = study_dag,
    text_size = 2
  )
}