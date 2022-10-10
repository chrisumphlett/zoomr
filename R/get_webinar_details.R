#' Get Webinar Details
#' 
#' Get metadata about a single webinar.
#' 
#' @param webinar_id Zoom Webinar Id, typically an 11 digit number.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_webinar_details(webinarID = "99911112222",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_webinar_details <- function(webinar_id,
                                account_id,
                                client_id,
                                client_secret)
  {
  
  . <- NA # prevent variable binding note for the dot
  
    # Get new access token
    access_token <- get_access_token(account_id, client_id, client_secret)

    # Function-specific API stuff
    api_url <- generate_url(query = "getwebinardetails",
                            webinar_id = webinar_id)

    # Send GET request to specific survey
    resp <- zoom_api_request(verb = "GET", url = api_url, token = access_token, query_params = "")
    
    # get into a data frame
    cnt <- httr::content(resp)
    cnt2 <- cnt[-13]
    df <- as.data.frame(cnt2) %>%
      janitor::clean_names()

}
