#' Get Top Terms for a Topic
#' 
#' Returns top-N terms for a topic ranked by probability.
#'
#' @param mod an object with STM or LDA model.
#' @param topic_n topic ID (numeric).
#' @param top_n_terms the number of terms to return.
#'
#' @returns a tibble with a ranked list of terms.
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
#' get_topN_terms(mod, 1, 100)
get_topN_terms <- function(mod, topic_n, top_n_terms) {
  ## extract top-N terms for a given topic (by probability)
  output <- mod |>
    tidytext::tidy() |>
    dplyr::filter(topic == topic_n) |>
    dplyr::slice_max(beta, n=top_n_terms)

  return(output)
}
