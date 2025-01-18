#' Pair-Wisely Compare Topic Solutions with Jaccard
#'
#' @param models A list object with STM models. Must be at least 2 models.
#' @param depth The number of top terms to use for comparison. Default is top 
#' 100 terms.
#'
#' @returns A tibble with model-to-model topic-to-topic Jaccard similarity.
#' @export
#'
#' @examples
#' library(stm)
#' library(dplyr)
#' 
#' modA <- stm(poliblog5k.docs, 
#'             poliblog5k.voc, K=25,
#'             prevalence=~rating, 
#'             data=poliblog5k.meta,
#'             max.em.its=2, 
#'             init.type="Random",
#'             seed = 9934) 
#'            
#' modB <- stm(poliblog5k.docs, 
#'             poliblog5k.voc, K=25,
#'             prevalence=~rating, 
#'             data=poliblog5k.meta,
#'             max.em.its=2, 
#'             init.type="Random",
#'             seed = 9576) 
#'            
#' compare_solutions(list(modA, modB), depth=100) |> 
#' arrange(desc(jaccard)) |> 
#' head()
compare_solutions <- function(models, depth = 100) {
    if (is.list(models) == FALSE) {
        print("Supply STM models in a list object")
    }
    if (length(models) < 2) {
        print("Need at least 2 models to search for stable topics")
    }
    k <- NULL
    for (i in seq(1, length(models))) {
        k <- c(k, models[[i]]$setting$dim$K)
    }
    k <- unique(k)
    if (length(k) > 1) {
        print("All models must have the same number of topics")
    }
    sol2sol_jacc <- NULL
    results <- data.frame(model_id_1 = NULL,
                          model_id_2 = NULL,
                          topic_id_1 = NULL,
                          topic_id_2 = NULL,
                          jaccard = NULL)
    for (i in seq(1, length(models))) {
        b <- i + 1
        if (b > length(models)) {
            print("Done")
        } else {
            for (j in seq(1, k)) {
                for (d in seq(1, k)) {

                    topic1 <- get_topN_terms(mod = models[[i]],
                                             topic_n = j,
                                             top_n_terms = depth) |>
                      dplyr::select(term) |> tibble::deframe()
                    topic2 <- get_topN_terms(mod = models[[b]],
                                             topic_n = d,
                                             top_n_terms = depth) |>
                      dplyr::select(term) |> tibble::deframe()
                    
                    sol2sol_jacc <- c(sol2sol_jacc,
                                      calcjaccard(topic1, topic2))
                    results <- rbind(results,
                                     data.frame(model_id_1 = paste("mod_",
                                                                   i,
                                                                   sep = ""),
                                                model_id_2 = paste("mod_",
                                                                   b,
                                                                   sep = ""),
                                                topic_id_1 = paste("topic_",
                                                                   j,
                                                                   sep = ""),
                                                topic_id_2 = paste("topic_",
                                                                   d,
                                                                   sep = ""),
                                                jaccard = sol2sol_jacc))
                }
            }
        }
    }
    return(results)
}
