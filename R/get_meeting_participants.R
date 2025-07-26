#' Get Meeting Participants
#'
#' Get participant information from all instances of a meeting. For recurring
#' meetings, this function automatically retrieves participants from all
#' occurrences, making it ideal for workshop attendance tracking.
#'
#' @param meeting_id Zoom Meeting ID (from list_meetings).
#' @param account_id Account ID granted by the Zoom developer app.
#' @param client_id Client ID granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' @param page_size Number of records per page. Default is 300.
#'
#' @details
#' \strong{Important Limitations:}
#' \itemize{
#'   \item Meeting instances older than 15 months cannot be retrieved
#'   \item This function works with all Zoom account types
#' }
#'
#' \strong{Enhanced Features for Pro+ Users:}
#' Users with Pro+ Zoom accounts can access additional Reports API endpoints
#' that provide more detailed participant analytics, custom date ranges,
#' and enhanced filtering capabilities.
#'
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect "everything"
#' @importFrom tidyselect "all_of"
#' @import dplyr
#' @importFrom janitor "clean_names"
#' @importFrom purrr "map_dfr"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#' @importFrom httr "GET"
#' @importFrom httr "add_headers"
#'
#' @return A data frame with data on each participant at a meeting. Includes
#' an 'instance_date' column to identify which occurrence each participant
#' attended (NA for non-recurring meetings).
#'
#' @seealso Official Zoom API documentation:
#' \url{https://developers.zoom.us/docs/api/meetings/#tag/meetings/get/past_meetings/\{meetingId\}/instances}
#' and \url{https://developers.zoom.us/docs/api/meetings/#tag/reports/get/report/meetings/\{meetingUuid\}/participants}
#' @export
#' @examples
#' \dontrun{
#' # Get participants from all instances of a meeting
#' dat <- get_meeting_participants(
#'   meeting_id = "81753923023",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret)
#' }

get_meeting_participants <- function(
  meeting_id,
  account_id,
  client_id,
  client_secret,
  page_size = 300
) {
  . <- NA # prevent variable binding note for the dot

  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)

  # Try to get all instances first (for recurring meetings)
  instances_url <- paste0(
    "https://api.zoom.us/v2/past_meetings/",
    meeting_id,
    "/instances"
  )

  response_instances <- httr::GET(
    url = instances_url,
    httr::add_headers(Authorization = paste("Bearer", access_token))
  )

  if (response_instances$status_code == 200) {
    instances_data <- jsonlite::fromJSON(
      httr::content(response_instances, "text")
    )

    if (length(instances_data$meetings) > 0) {
      # This is a recurring meeting with multiple instances
      instances_df <- as.data.frame(instances_data$meetings)
      all_participants <- data.frame()

      # Get participants for each instance
      for (i in 1:nrow(instances_df)) {
        uuid_encoded <- URLencode(instances_df$uuid[i], reserved = TRUE)
        participants_url <- paste0(
          "https://api.zoom.us/v2/report/meetings/",
          uuid_encoded,
          "/participants"
        )

        elements <- list()
        next_token <- ""

        # Handle pagination for this instance
        while (next_token != "STOP") {
          resp <- httr::GET(
            url = participants_url,
            httr::add_headers(Authorization = paste("Bearer", access_token)),
            query = list(page_size = page_size, next_page_token = next_token)
          )

          if (resp$status_code == 200) {
            resp_content <- httr::content(resp, "text")
            resp_data <- jsonlite::fromJSON(resp_content, flatten = TRUE)
            next_token <- if (
              is.null(resp_data$next_page_token) ||
                resp_data$next_page_token == ""
            )
              "STOP" else resp_data$next_page_token
            elements <- append(elements, resp_content)
          } else {
            next_token <- "STOP"
          }
        }

        if (length(elements) > 0) {
          # Parse participants for this instance
          list_to_df <- function(.x) {
            participants_data <- jsonlite::fromJSON(.x, flatten = TRUE)
            if (
              !is.null(participants_data$participants) &&
                length(participants_data$participants) > 0
            ) {
              df <- as.data.frame(participants_data$participants) %>%
                dplyr::mutate(dplyr::across(
                  .cols = tidyselect::everything(),
                  as.character
                ))
              return(df)
            } else {
              return(data.frame())
            }
          }

          instance_participants <- purrr::map_dfr(elements, list_to_df)

          if (nrow(instance_participants) > 0) {
            # Add instance information and standardize column names
            instance_participants$instance_date <-
              format(as.Date(instances_df$start_time[i]), "%Y-%m-%d")
            instance_participants$instance_start_time <-
              instances_df$start_time[i]

            # Standardize column names to match original function
            if ("name" %in% names(instance_participants)) {
              instance_participants$participants_name <-
                instance_participants$name
            }
            if ("duration" %in% names(instance_participants)) {
              instance_participants$participants_duration <-
                instance_participants$duration
            }
            if ("join_time" %in% names(instance_participants)) {
              instance_participants$participants_join_time <-
                instance_participants$join_time
            }
            if ("leave_time" %in% names(instance_participants)) {
              instance_participants$participants_leave_time <-
                instance_participants$leave_time
            }
            if ("user_email" %in% names(instance_participants)) {
              instance_participants$participants_user_email <-
                instance_participants$user_email
            }

            all_participants <- rbind(all_participants, instance_participants)
          }
        }
      }

      if (nrow(all_participants) > 0) {
        df <- all_participants %>%
          janitor::clean_names()

        # Remove pagination columns only if they exist
        pagination_cols <- c(
          "page_count",
          "page_size",
          "next_page_token",
          "total_records"
        )
        existing_pagination_cols <- intersect(pagination_cols, names(df))

        if (length(existing_pagination_cols) > 0) {
          df <- df %>% dplyr::select(-all_of(existing_pagination_cols))
        }

        return(df)
      }
    }
  }

  # Fallback: single meeting (non-recurring or instances endpoint failed)
  api_url <- generate_url(
    query = "getmeetingparticipants",
    meeting_id = meeting_id
  )

  elements <- list()

  next_token <- ""
  while (next_token != "STOP") {
    resp <- zoom_api_request(
      verb = "GET",
      url = api_url,
      token = access_token,
      query_params = list(page_size = page_size, next_page_token = next_token)
    )
    resp2 <- jsonlite::fromJSON(httr::content(resp, "text"), flatten = TRUE)
    next_token <- dplyr::if_else(
      resp2$next_page_token == "",
      "STOP",
      resp2$next_page_token
    )
    elements <- append(elements, httr::content(resp, "text"))
  }

  list_to_df <- function(.x) {
    df <- as.data.frame(jsonlite::fromJSON(.x, flatten = TRUE)) %>%
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::everything(),
        as.character
      ))
  }

  df <- purrr::map_dfr(elements, list_to_df) %>%
    janitor::clean_names()

  # Remove pagination columns only if they exist
  pagination_cols <- c(
    "page_count",
    "page_size",
    "next_page_token",
    "total_records"
  )
  existing_pagination_cols <- intersect(pagination_cols, names(df))

  if (length(existing_pagination_cols) > 0) {
    df <- df %>% dplyr::select(-all_of(existing_pagination_cols))
  }

  # Add instance_date column (NA for non-recurring meetings)
  if (!"instance_date" %in% names(df)) {
    df$instance_date <- NA_character_
  }

  return(df)
}
