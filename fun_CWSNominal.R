#' @title Calculate CWS Index for Nominal Data
#'
#' @description This function calculates the Cochra-Weiss-Shanteau (CWS) index
#' The CWS Index is a measure of the balance between within-subject consistency
#' and between-subject discrimination. The purpose of the index is to provide a
#' single score that can be used to compare the consistency and discrimination
#' of different measurement instruments or time points.
#'
#' @param input_data A data frame where each row represents an item and each
#'   column represents a time point of measurement. The data should be nominal
#'   (character or factor).
#'
#' @return A numeric value representing the CWS Index.
#'
#' @author Don C. Zhang, dc.zhang1@gmail.com 
#' @citation: Zhang, D. C., & Wang, Y. (2021). An empirical approach to 
#' identifying subject matter experts for the development of situational 
#' judgment tests. Journal of Personnel Psychology.
#'
#' @examples
#' # Example dataframe with 5 items and 2 test administrations
#' input_data <- data.frame(
#'   time1 = c("A", "A", "B", "D", "C"),
#'   time2 = c("A", "B", "D", "D", "C")
#' )
#'
#' # Calculate CWS Index using the function
#' cws_result <- calculate_cws_index(input_data)
#' print(cws_result)
#'
#' @importFrom utils combn
calculate_cws_index <- function(input_data) {
  # Ensure input is a data frame
  if (!is.data.frame(input_data)) {
    stop("Input must be a data frame.")
  }

  # Convert all columns to character to handle factors consistently
  input_data <- as.data.frame(lapply(input_data, as.character), stringsAsFactors = FALSE)

  # --- Main data parameters ---
  num_items <- nrow(input_data)
  num_time_points <- ncol(input_data)
  total_responses <- num_items * num_time_points

  # Calculate total possible pairs of responses across all items and time points
  total_possible_pairs <- choose(total_responses, 2)

  # --- Calculating inconsistency (within-item variability) ---
  # Possible pairs of responses within each item across time points
  possible_within_item_pairs <- choose(num_time_points, 2) * num_items

  # Calculate observed matches within each item across time points
  observed_within_item_matches <- 0
  for (i in 1:num_items) {
    # Get responses for the current item across all time points
    item_responses <- input_data[i, ]

    # Calculate the frequency of each response category for the item
    response_counts <- table(item_responses)

    # Sum the number of pairs of identical responses for the item
    # choose(n, 2) calculates the number of pairs for a response category with n occurrences
    observed_within_item_matches <- observed_within_item_matches + sum(choose(response_counts, 2))
  }

  # Inconsistency Index: 1 - (Observed within-item matches / Possible within-item pairs)
  # This measures the proportion of possible within-item pairs that are NOT matches.
  inconsistency_index <- 1 - (observed_within_item_matches / possible_within_item_pairs)

  # --- Calculating discrimination (between-item variability) ---
  # Possible pairs of responses across different items (between-item pairs)
  possible_between_item_pairs <- total_possible_pairs - possible_within_item_pairs

  # Gather all responses from all items and time points into a single vector
  all_responses <- unlist(input_data)

  # Calculate total observed matches across all responses
  # This includes both within-item and between-item matches
  total_observed_matches <- sum(choose(table(all_responses), 2))

  # Observed matches between items = Total observed matches - Observed within-item matches
  observed_between_item_matches <- total_observed_matches - observed_within_item_matches

  # Discrimination Index: (Possible between-item pairs - Observed between-item matches) / Possible between-item pairs
  # This measures the proportion of possible between-item pairs that are NOT matches.
  discrimination_index <- (possible_between_item_pairs - observed_between_item_matches) / possible_between_item_pairs

  # --- Calculate CWS Index ---
  # CWS Index = Discrimination Index / Inconsistency Index
  # Add a check for inconsistency_index being zero to avoid division by zero
  cws_index <- ifelse(inconsistency_index == 0, Inf, discrimination_index / inconsistency_index)

  return(cws_index)
}

# --- Using the function with the example data ---
inputData <- data.frame(
  time1 = c("A", "A", "B", "D", "C"),
  time2 = c("A", "B", "D", "D", "C")
)

cws_result <- calculate_cws_index(inputData)
print(cws_result)