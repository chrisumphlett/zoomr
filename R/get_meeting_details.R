#' Get Meeting Details
#' 
#' Get metadata about a single meeting including participant count, 
#' duration, and other meeting statistics.
#' 
#' @param meeting_id Zoom Meeting ID (from list_meetings).
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @importFrom janitor "clean_names"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @return A data frame with the metadata about a meeting.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_meeting_details(meeting_id = "81753923023",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_meeting_details <- function(meeting_id,
                                account_id,
                                client_id,
                                client_secret)
{
  
  . <- NA # prevent variable binding note for the dot
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "getmeetingdetails",
                          meeting_id = meeting_id)
  
  # Send GET request to specific meeting
  resp <- zoom_api_request(verb = "GET", 
                           url = api_url, 
                           token = access_token, 
                           query_params = "")
  
  # Send GET request to specific meeting
  resp <- zoom_api_request(verb = "GET", 
                           url = api_url, 
                           token = access_token, 
                           query_params = "")
  
  # Parse JSON content
  parsed_data <- httr::content(resp)
  
  # Remove problematic list columns (like tracking_fields)
  # Keep only simple data types that can be converted to data.frame
  simple_data <- parsed_data[sapply(parsed_data, function(x) !is.list(x))]
  
  # Convert to data frame
  df <- as.data.frame(simple_data, stringsAsFactors = FALSE) %>%
    janitor::clean_names()
  
  return(df)
}