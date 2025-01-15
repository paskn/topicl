#' Calculate Jaccard Similarity for a Pair of Topics
#' 
#' A primary internal function for calculating Jaccard similarity for a given 
#' pair of topics.
#'
#' @param topic1 a vector with terms most representative of a topic.
#' @param topic2 a vector with terms most representative of another topic.
#'
#' @returns a value of Jaccard similarity for a pair of topics (numerical).
#' @export
#'
#' @examples
#' topicA <- c("tax", "money", "million", "financi", "govern", "spend", "billion")
#' topicB <- c("iraq", "war", "militari", "iraqi", "troop", "forc", "will")
#' 
#' calcjaccard(topicA, topicB)
calcjaccard <- function(topic1, topic2) {
  # Calculate the size of the intersection and union of the sets
  intersect_size <- length(intersect(topic1, topic2))
  union_size <- length(union(topic1, topic2))
  
  # Calculate and return the Jaccard similarity
  jaccard_similarity <- intersect_size / union_size
  return(jaccard_similarity)
}
