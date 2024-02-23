#' Get Webinar Tracking Sources
#' 
#' Get a summary of registrations and visitors by tracking source for a 
#' specific webinar.
#' 
#' @param webinar_id Zoom Webinar Id, typically an 11 digit number.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom janitor "clean_names"
#' @import dplyr
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @return A data frame with the list of panelists from that webinar.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_tracking_sources(webinar_id = "99911112222",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_tracking_sources <- function(webinar_id,
                          account_id,
                          client_id,
                          client_secret)
{
  
  . <- NA # prevent variable binding note for the dot
  
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "gettrackingsources",
                          webinar_id = webinar_id)
  
  # Send GET request to specific survey
  resp <- zoom_api_request(verb = "GET", url = api_url, token = access_token, query_params = "")
  
  df <- as.data.frame(jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)) %>%
    janitor::clean_names() %>%
    dplyr::select(-c("tracking_sources_tracking_url", "total_records")) %>%
    dplyr::rename(visitor_count = "tracking_sources_visitor_count",
           registration_count = "tracking_sources_registrationr_count",
           tracking_source_name = "tracking_sources_source_name",
           tracking_source_id = "tracking_sources_id") %>%
    dplyr::mutate(webinar_id = webinar_id) %>%
    dplyr::relocate(webinar_id)
}
