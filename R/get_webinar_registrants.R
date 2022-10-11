#' Get Webinar Registrants
#' 
#' Get registrant info about a single webinar.
#' 
#' @param webinar_id Zoom Webinar Id, typically an 11 digit number.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect "everything"
#' @import dplyr
#' @importFrom janitor "clean_names"
#' @importFrom purrr "map_dfr"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_webinar_registrants(webinarID = "99911112222",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_webinar_registrants <- function(webinar_id,
                                     account_id,
                                     client_id,
                                     client_secret)
{
  
  . <- NA # prevent variable binding note for the dot
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "getwebinarregistrants",
                          webinar_id = webinar_id)
  
  # api_query_params <- generate_query_params(query = "getwebinarregistrants")
  # message(api_query_params)
  
  elements <- list()
  
  next_token <- ""
  skip <- ""
  while (next_token != "STOP") {
    resp <- zoom_api_request(verb = "GET",
                             url = api_url,
                             token = access_token,
                             query_params = list(page_size = 300,
                                                 next_page_token = next_token)#api_query_params
    )
    if(jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)$total_records == 0) {
      message("Webinar Id is found but there are not any registrants")
      next_token <- "STOP"
      skip <- "YES"
    } else {
      resp2 <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)
      next_token <- dplyr::if_else(resp2$next_page_token == "", "STOP", resp2$next_page_token)
      elements <- append(elements, httr::content(resp, "text"))
      skip <- "NO"
    }
  }
  
  if(skip != "YES"){
    list_to_df <- function(.x) {
      df <- as.data.frame(jsonlite::fromJSON(.x, flatten = TRUE)) %>%
        dplyr::mutate(dplyr::across(.cols = tidyselect::everything(), as.character))
    }
    df <- purrr::map_dfr(elements, list_to_df) %>%
      janitor::clean_names() %>%
      dplyr::select(-c(
        .data$registrants_custom_questions,
        .data$page_size,
        .data$next_page_token,
        .data$total_records
      ))
    return(df)
  }
}
