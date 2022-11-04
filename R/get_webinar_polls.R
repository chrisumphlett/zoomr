#' Get Webinar Polls
#' 
#' Get the polls summary from a single webinar.
#' 
#' @param webinar_id Zoom Webinar Id, typically an 11 digit number.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tidyr "unnest"
#' @importFrom janitor "clean_names"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @return A data frame with poll results from a webinar.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_webinar_polls(webinar_id = "99911112222",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_webinar_polls <- function(webinar_id,
                              account_id,
                              client_id,
                              client_secret)
{
  
  . <- NA # prevent variable binding note for the dot
  
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "getwebinarpolls",
                          webinar_id = webinar_id)
  
  # Send GET request to specific survey
  resp <- zoom_api_request(verb = "GET", url = api_url, token = access_token, query_params = "")
  
  # check if it is empty (applicable data not available for this webinar)
  if(length(httr::content(resp)$questions) == 0){
    message("Zoom API returned no poll results for this webinar")
    return(list())
  } else {
    df <- as.data.frame(jsonlite::fromJSON(
      httr::content(resp, "text"),
      flatten = TRUE
    )
    ) %>%
      tidyr::unnest("questions.question_details") %>%
      janitor::clean_names()
    return(df)
  }
}
