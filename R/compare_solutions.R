#' Pair-Wisely Compare Topic Solutions with Jaccard
#'
#' @param models A list object with STM models. Must be at least 2 models.
#' @param depth The number of top terms to use for comparison. Default is top 
#' 100 terms.
#'
#' @returns A tibble with model-to-model topic-to-topic Jaccard similarity.
#' @importFrom rlang .data
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
        dplyr::select(.data$term) |>
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
#' @importFrom rlang .data
#' @export
filter_topics <- function(res, sim_threshold, n_sols) {
  # Filter results by Jaccard similarity and focus on model_1 topics
  filtered_results <- res |> 
    dplyr::filter(.data$jaccard >= sim_threshold) |> 
    dplyr::mutate(
      # Create identifiers for topics and models
      topic_1 = paste(.data$model_id_A, .data$topic_id_A, sep = "_"),
      topic_2 = paste(.data$model_id_B, .data$topic_id_B, sep = "_")
    ) |> 
    dplyr::filter(.data$model_id_A == "mod_1")  # Only keep pairs involving model_1 topics

  # Count how many distinct models each topic from model_1 is similar to
  topic_summary <- filtered_results |>
    dplyr::group_by(.data$topic_1) |>
    dplyr::reframe(
      distinct_models = dplyr::n_distinct(.data$model_id_B)  # Count unique models in model_2
    )

  # Filter topics that appear in at least 2 other models
  relevant_topics <- topic_summary |>
    dplyr::filter(.data$distinct_models >= n_sols)

  return(relevant_topics)
}

#' Visualize topic similarities with a network
#'
#' @param res output of `compare_solutions()`
#' @param sim_threshold Jaccard similarity threshold for filtering.
#'
#' @returns ggraph plot.
#' @importFrom rlang .data
#' @export
viz_comparisons <- function(res, sim_threshold) {
  #library(igraph)
  #library(ggraph)
  #library(tidygraph)
  #library(dplyr)

  filtered_results <- res |> 
    dplyr::filter(.data$jaccard >= sim_threshold) |> 
    dplyr::mutate(
      # Create identifiers for topics and models
      topic_1 = paste(.data$model_id_A, .data$topic_id_A, sep = "_"),
      topic_2 = paste(.data$model_id_B, .data$topic_id_B, sep = "_")
    ) |> 
    dplyr::filter(.data$model_id_A == "mod_1")  # Only keep pairs involving model_1 topics

  # Prepare edge list (connections between topics)
  edges <- filtered_results |>
    dplyr::select(.data$topic_1, .data$topic_2, .data$jaccard)

  # Prepare node list (unique topics with model information)
  nodes <- filtered_results |>
    dplyr::select(.data$topic_1, .data$model_id_A) |>
    dplyr::rename(topic = .data$topic_1, model = .data$model_id_A) |>
    dplyr::distinct() |>
    dplyr::bind_rows(
      filtered_results |>
        dplyr::select(.data$topic_2, .data$model_id_B) |>
        dplyr::rename(topic = .data$topic_2, model = .data$model_id_B) |>
        dplyr::distinct()
    ) |>
    dplyr::distinct()

  # Convert nodes to a factor for consistent coloring
  nodes <- nodes |>
    dplyr::mutate(model = as.factor(.data$model))

  # Create the graph object
  graph <- tidygraph::tbl_graph(nodes = nodes,
                     edges = edges,
                     directed = FALSE)

  # Visualize the graph
  ggraph::ggraph(graph, layout = "fr") +  # 'fr' = Fruchterman-Reingold layout
    ggraph::geom_edge_link(ggplot2::aes(alpha = .data$jaccard),
                           color = "gray") +  # Edges
    ggraph::geom_node_point(ggplot2::aes(color = .data$model),
                            alpha = 0.9) +  # Nodes
    ggraph::geom_node_text(ggplot2::aes(label = .data$topic),
                           repel = TRUE, size = 3) +  # Labels
    ggplot2::scale_color_brewer(palette = "Set2") +  # Distinct colors for models
    ggplot2::theme_void() +
    ggplot2::labs(title = "Topic Similarity Network",
         subtitle = "Topics from model_1 connected to others by Jaccard similarity",
         edge_width = "Jaccard Index") +
    ggplot2::theme(legend.position = "bottom")
  
}
