#' Get topic coherence from Palmetto remote or local
#'
#' @param solution A wide tibble where each column contains a top-N terms.
#' @param depth The number of top terms to use to use for coherence estimation. If `backend = "web"`, then the number cannot be less than 3 and more than 10.
#' @param coherence A name of coherence metric. Valid names: "ca", "cp", "cv", "npmi", "uci", "umass". Default is `npmi`.
#' @param backend Path to Palmetto either `"web"` or a string with an URL. Example: http://localhost:7777/service/.
#'
#' @returns A wide tibble with requested coherence scores.
#' @export
#'
#' @examples
#' df1 <- tibble(col1 = c("apple", "banana", "cherry"), col2 = c("dog", "cat", "mouse"))
#' 
#' ask_palmetto(df1)
ask_palmetto <- function(solution, depth = "full", coherence = "npmi", backend = "web") {
  if (depth == "full") {
    depth <- nrow(solution)
  }
  if (depth < 3) {
    stop("Topic or depth must be of 3 or more terms.")
  }
  if (backend == "web") {
     backend <- "https://palmetto.demos.dice-research.org/service/"

    if (depth > 10) {
      warning("Palmetto web service cannot accept more than 10 terms. Use local service. Using 10 terms instead.")
      depth <- 10
    }
  }
  palmetto_path <- backend
  ## TODO: rewrite for the type format of input
  reqs <- list()
  ids <- c()
  ## construct requests
  for (i in seq_along(solution)) {
    topic_id <- names(solution[i])
    ids <- c(ids, topic_id)
    topic_terms <- paste(tibble::deframe(solution[1:depth,i]),
                         collapse = "+")
    
    req <- paste(palmetto_path,
                 coherence,
                 "?words=",
                 topic_terms,
                 sep = "") |>
      request()
    ## |> 
      ## req_retry(retry_on_failure = TRUE)

    reqs <- c(reqs, list(req))
  }
  
  ## dispatch requests
  resps <- reqs |>
    req_perform_parallel(on_error = "continue",
                         progress = paste("fetching",
                                          coherence, "from",
                                          sub("^https?://([^/]+)/.*", "\\1",
                                              backend)))
  names(resps) <- ids
  out <- resps |>
    map(c(resp_body_string, as.numeric)) |>
    as_tibble()
  
  return(out)
}
