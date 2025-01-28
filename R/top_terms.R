#' Get Top Terms for a Topic
#' 
#' Returns top-N terms for a topic ranked by probability.
#'
#' @param mod an object with STM or LDA model.
#' @param topic_id topic ID (numeric).
#' @param n_terms the number of terms to return.
#'
#' @returns A tibble with a ranked list of terms.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' library(stm)
#' 
#' mod <- stm(poliblog5k.docs, 
#'            poliblog5k.voc, K=25,
#'            prevalence=~rating, 
#'            data=poliblog5k.meta,
#'            max.em.its=2, 
#'            init.type="Spectral") 
#' 
#' top_terms(mod, 1, 100)
top_terms <- function(mod, topic_id, n_terms) {
  ## extract top-N terms for a given topic (by probability)
  if (class(mod) == "STM") {
    output <- mod |>
      tidytext::tidy(matrix = "beta",
                     log = FALSE) |>
      dplyr::filter(.data$topic == topic_id) |>
      dplyr::slice_max(beta, n=n_terms)

    return(output)
  } else {
    stop("Can take only STM model for now.")
  }
  
  
}
