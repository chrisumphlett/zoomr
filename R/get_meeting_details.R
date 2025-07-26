#' Get Meeting Details
#'
#' Get metadata about a single meeting including participant count,
#' duration, and other meeting statistics.
#'
#' @param meeting_id Zoom Meeting ID (from list_meetings).
#' @param account_id Account ID granted by the Zoom developer app.
#' @param client_id Client ID granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#'
#' @details
#' This function retrieves basic meeting metadata. For more comprehensive
#' meeting analytics and participant data, users with Pro+ Zoom accounts
#' can access the Reports API endpoints which provide enhanced meeting
#' details and participant information.
#'
#' @importFrom janitor "clean_names"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#'
#' @return A data frame with the metadata about a meeting. The \code{type}
#' column contains integer codes describing the meeting structure/format,
#' while \code{meeting_format_label} provides human-readable descriptions:
#' \itemize{
#'   \item 1 = Instant meeting
#'   \item 2 = Scheduled meeting
#'   \item 3 = Recurring meeting (no fixed time)
#'   \item 8 = Recurring meeting (fixed time)
#'   \item 10 = Screen share only meeting
#' }
#'
#' @seealso Official Zoom API documentation:
#' \url{https://developers.zoom.us/docs/api/meetings/#tag/meetings/get/meetings/\{meetingId\}}
#' @export
#' @examples
#' \dontrun{
#' dat <- get_meeting_details(meeting_id = "81753923023",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_meeting_details <- function(
  meeting_id,
  account_id,
  client_id,
  client_secret
) {
  . <- NA # prevent variable binding note for the dot

  # Meeting format labels mapping
  meeting_format_labels <- get_meeting_format_labels()

  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)

  # Function-specific API stuff
  api_url <- generate_url(query = "getmeetingdetails", meeting_id = meeting_id)

  # Send GET request to specific meeting
  resp <- zoom_api_request(
    verb = "GET",
    url = api_url,
    token = access_token,
    query_params = ""
  )

  # Send GET request to specific meeting
  resp <- zoom_api_request(
    verb = "GET",
    url = api_url,
    token = access_token,
    query_params = ""
  )

  # Parse JSON content
  parsed_data <- httr::content(resp)

  # Remove problematic list columns (like tracking_fields)
  # Keep only simple data types that can be converted to data.frame
  simple_data <- parsed_data[sapply(parsed_data, function(x) !is.list(x))]

  # Convert to data frame
  df <- as.data.frame(simple_data, stringsAsFactors = FALSE) %>%
    janitor::clean_names()

  # Add meeting format labels
  if ("type" %in% names(df)) {
    df$meeting_format_label <- meeting_format_labels[as.character(df$type)]
    # Set NA for any unknown meeting types
    df$meeting_format_label[is.na(df$meeting_format_label)] <- "Unknown"
  }

  return(df)
}
