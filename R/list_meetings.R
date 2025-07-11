#' Get List of Meetings for a User
#' 
#' Get list of meetings for a User. This is used to get the meeting Id's and UUIDs to
#' pass into other functions.
#' 
#' @param user_id Zoom User Id.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' @param meeting_type Type of meetings to retrieve. Options are "scheduled", 
#' "live", "upcoming", or "previous_meetings". Default is "previous_meetings".
#' @param page_size Number of records per page. Default is 300.
#' 
#' @importFrom dplyr "select"
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect "everything"
#' @import dplyr
#' @importFrom janitor "clean_names"
#' @importFrom purrr "map_dfr"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @return A data frame with all of the meetings hosted by a specific user.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- list_meetings(user_id = "user_id_string",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret,
#'   meeting_type = "previous_meetings")
#' }

list_meetings <- function(user_id,
                          account_id,
                          client_id,
                          client_secret,
                          meeting_type = "previous_meetings",
                          page_size = 300)
{
  
  . <- NA # prevent variable binding note for the dot
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "listmeetings",
                          user_id = user_id)
  
  elements <- list()
  
  next_token <- ""
  while (next_token != "STOP") {
    resp <- zoom_api_request(verb = "GET",
                             url = api_url,
                             token = access_token,
                             query_params = list(page_size = page_size,
                                                 next_page_token = next_token,
                                                 type = meeting_type)
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
      "page_size",
      "next_page_token", 
      "total_records"
    ))
  return(df)
}