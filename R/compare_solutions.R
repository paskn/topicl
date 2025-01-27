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
  # Input validation
  if (!is.list(models)) stop("Supply STM models in a list object")
  if (length(models) < 2) stop("Need at least 2 models to search for stable topics")

  # Check for consistent number of topics
  k <- unique(sapply(models, function(mod) mod$setting$dim$K))
  if (length(k) > 1) stop("All models must have the same number of topics")
  k <- k[1]

  # Precompute top terms for all topics
  top_terms_list <- lapply(models, function(mod) {
    lapply(seq_len(k), function(topic_id) {
      top_terms(mod = mod, topic_id = topic_id, n_terms = depth) |>
        dplyr::select(term) |>
        tibble::deframe()
    })
  })

  # Precompute model pairs
  model_pairs <- utils::combn(seq_along(models), 2, simplify = FALSE)

  # Initialize results storage
  results_list <- vector("list", length = length(model_pairs) * k^2)
  index <- 1  # Track the position in results_list

  # Set up progress bar
  cli::cli_progress_bar(name = "comparing solutions",
                        total = length(model_pairs) * k^2)

  # Compare models
  for (pair in model_pairs) {
    model_1 <- pair[1]
    model_2 <- pair[2]

    for (topic_1 in seq_len(k)) {
      for (topic_2 in seq_len(k)) {
        # Retrieve precomputed top terms
        terms_1 <- top_terms_list[[model_1]][[topic_1]]
        terms_2 <- top_terms_list[[model_2]][[topic_2]]

        # Compute Jaccard similarity
        jacc <- calcjaccard(terms_1, terms_2)

        # Store result
        results_list[[index]] <- tibble::tibble(
          model_id_A = paste0("mod_", model_1),
          topic_id_A = paste0("topic_", topic_1),
          model_id_B = paste0("mod_", model_2),
          topic_id_B = paste0("topic_", topic_2),
          jaccard = jacc
        )
        index <- index + 1

        # Update progress bar
        cli::cli_progress_update()
      }
    }
  }

  # Close progress bar
  cli::cli_progress_done()

  # Combine results into a single tibble
  results <- dplyr::bind_rows(results_list)

  return(results)
}

#' Summarize solution comparisons
#'
#' @param res output from `compare_solutions()`
#' @param sim_threshold level of Jaccard similarity for filtering stable topics.
#' @param n_sols the number of fits to decide if a topic is stable.
#'
#' @returns tibble with counts of topic persistence across fits.
#' @export
filter_topics <- function(res, sim_threshold, n_sols) {
  # Filter results by Jaccard similarity and focus on model_1 topics
  filtered_results <- res |> 
    dplyr::filter(jaccard >= sim_threshold) |> 
    dplyr::mutate(
      # Create identifiers for topics and models
      topic_1 = paste(model_id_A, topic_id_A, sep = "_"),
      topic_2 = paste(model_id_B, topic_id_B, sep = "_")
    ) |> 
    dplyr::filter(model_id_A == "mod_1")  # Only keep pairs involving model_1 topics

  # Count how many distinct models each topic from model_1 is similar to
  topic_summary <- filtered_results |>
    dplyr::group_by(topic_1) |>
    dplyr::reframe(
      distinct_models = dplyr::n_distinct(model_id_B)  # Count unique models in model_2
    )

  # Filter topics that appear in at least 2 other models
  relevant_topics <- topic_summary |>
    dplyr::filter(distinct_models >= n_sols)

  return(relevant_topics)
}
}
