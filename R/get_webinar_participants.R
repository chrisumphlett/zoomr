#' Get Webinar Participants
#' 
#' Get participant info about a single webinar.
#' 
#' @param webinar_id Zoom Webinar Id, typically an 11 digit number.
#' @param account_id Account Id granted by the Zoom developer app.
#' @param client_id Client Id granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' 
#' @seealso See <https://marketplace.zoom.us/docs/api-reference/zoom-api/> for 
#' documentation on the Zoom API.
#' @export
#' @examples
#' \dontrun{
#' dat <- get_webinar_participants(webinarID = "99911112222",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_webinar_participants <- function(webinar_id,
                                account_id,
                                client_id,
                                client_secret)
{
  
  . <- NA # prevent variable binding note for the dot
  
  
  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)
  
  # Function-specific API stuff
  api_url <- generate_url(query = "getwebinarparticipants",
                          webinar_id = webinar_id)
  
  # api_query_params <- generate_query_params(query = "getwebinarparticipants")
  # message(api_query_params)
  
  elements <- list()
  
  page_counter <- 1
  
  total_pages <- 1
  
  # while (page_counter <= total_pages) {
    # Send GET request to specific survey
  # message(paste0(zoom_api_request(verb = "GET",
  #                      url = api_url,
  #                      token = access_token,
  #                      query_params = api_query_params
  # )))
    resp <- zoom_api_request(verb = "GET",
                             url = api_url,
                             token = access_token,
                             query_params = list(page_size = 300)#api_query_params
                             )
    # elements <- append(elements, resp)
    # page_counter <- page_counter + 1
    # total_pages <- 10
    
  # }
  

    # get into a data frame
    df <- as.data.frame(jsonlite::fromJSON(
                          httr::content(resp, "text"),
                          flatten = TRUE
                          )
                        ) %>%
      janitor::clean_names() %>%
      dplyr::select(-c(
        .data$page_size,
        .data$next_page_token,
        .data$total_records
                      )
                    )
#}
 return(df) 
}
