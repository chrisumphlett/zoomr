#' Get Meeting Participants
#' 
#' Get participant info about a single meeting.
#' 
#' @param meeting_id Zoom Meeting ID (from list_meetings). Note: Use meeting_id, not UUID.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' @param page_size Number of records per page. Default is 300.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect "everything"
#' @import dplyr
#' @importFrom janitor "clean_names"
#' @importFrom purrr "map_dfr"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @return A data frame with data on each participant at a meeting.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_meeting_participants(meeting_id = "81753923023",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_meeting_participants <- function(meeting_id,
                                     account_id,
                                     client_id,
                                     client_secret,
                                     page_size = 300)
{
  
  . <- NA # prevent variable binding note for the dot
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "getmeetingparticipants",
                          meeting_id = meeting_id)
  
  elements <- list()
  
  next_token <- ""
  while (next_token != "STOP") {
    resp <- zoom_api_request(verb = "GET",
                             url = api_url,
                             token = access_token,
                             query_params = list(page_size = page_size,
                                                 next_page_token = next_token)
    )
    resp2 <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)
    next_token <- dplyr::if_else(resp2$next_page_token == "", 
                                 "STOP", 
                                 resp2$next_page_token)
    elements <- append(elements, httr::content(resp, "text"))
  }
  
  list_to_df <- function(.x) {
    df <- as.data.frame(jsonlite::fromJSON(.x, flatten = TRUE)) %>%
      dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), as.character))
  }
  
  df <- purrr::map_dfr(elements, list_to_df) %>%
    janitor::clean_names() %>%
    dplyr::select(-c(
      "page_count",
      "page_size",
      "next_page_token",
      "total_records"
    ))
  return(df)
}