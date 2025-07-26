#' Get List of Meetings for a User
#'
#' Get list of meetings for a User. This function retrieves meeting IDs and
#' UUIDs to pass into other functions for detailed analysis.
#'
#' @param user_id Zoom User ID or email address.
#' @param account_id Account ID granted by the Zoom developer app.
#' @param client_id Client ID granted by the Zoom developer app.
#' @param client_secret Client secret granted by the Zoom developer app.
#' @param meeting_type Type of meetings to retrieve. Options are:
#' \itemize{
#'   \item "previous_meetings" = All previous meetings (default)
#'   \item "scheduled" = All valid previous, live, and upcoming scheduled
#'   meetings
#'   \item "upcoming" = All upcoming meetings, including live meetings
#'   \item "upcoming_meetings" = All upcoming meetings, including live meetings
#'   \item "live" = Currently ongoing meetings
#' }
#' @param page_size Number of records per page. Maximum 300, default 300.
#'
#' @details
#' \strong{Important Limitations:}
#' \itemize{
#'   \item \strong{6-month historical limit:} When using "upcoming",
#'   "upcoming_meetings",
#'   or "previous_meetings", only meetings from the last 6 months are returned
#'   \item \strong{Scheduled meetings only:} Does not return instant meetings
#'   \item \strong{Unexpired meetings only:} Only returns meetings that
#'   haven't expired
#' }
#'
#' \strong{For Enhanced Historical Data Access:}
#' Users with Pro+ Zoom accounts can access the Reports API endpoint
#' \code{/report/users/\{userId\}/meetings} which offers:
#' \itemize{
#'   \item Date range filtering with \code{from} and \code{to} parameters
#'   \item Enhanced meeting details including participant information
#'   \item Better historical data retrieval capabilities
#' }
#'
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect "everything"
#' @importFrom tidyselect "all_of"
#' @import dplyr
#' @importFrom janitor "clean_names"
#' @importFrom purrr "map_dfr"
#' @importFrom jsonlite "fromJSON"
#' @importFrom httr "content"
#'
#' @return A data frame with meetings hosted by the specified user.
#' The \code{meetings_type} column contains integer codes describing the
#' meeting structure/format, while \code{meeting_format_label} provides
#' human-readable descriptions:
#' \itemize{
#'   \item 1 = Instant meeting
#'   \item 2 = Scheduled meeting
#'   \item 3 = Recurring meeting (no fixed time)
#'   \item 8 = Recurring meeting (fixed time)
#'   \item 10 = Screen share only meeting
#' }
#'
#' @seealso Official Zoom API documentation:
#' \url{https://developers.zoom.us/docs/api/meetings/#tag/meetings/get/users/\{userId\}/meetings}
#' @export
#' @examples
#' \dontrun{
#' dat <- list_meetings(user_id = "user_id_string",
#'   your_account_id,
#'   your_client_id,
#'   your_client_secret,
#'   meeting_type = "previous_meetings")
#' }

list_meetings <- function(
  user_id,
  account_id,
  client_id,
  client_secret,
  meeting_type = "previous_meetings",
  page_size = 300
) {
  . <- NA # prevent variable binding note for the dot

  # Validate meeting_type parameter
  valid_types <- c(
    "scheduled",
    "live",
    "upcoming",
    "upcoming_meetings",
    "previous_meetings"
  )
  if (!meeting_type %in% valid_types) {
    rlang::abort(
      c(
        "Invalid meeting_type parameter",
        "i" = paste(
          "meeting_type must be one of:",
          paste(valid_types, collapse = ", ")
        ),
        "x" = paste("You provided:", meeting_type)
      )
    )
  }

  # Validate page_size parameter
  if (page_size < 1 || page_size > 300) {
    rlang::abort(
      c(
        "Invalid page_size parameter",
        "i" = "page_size must be between 1 and 300",
        "x" = paste("You provided:", page_size)
      )
    )
  }

  # Meeting format labels mapping
  meeting_format_labels <- get_meeting_format_labels()

  # Get new access token
  access_token <- get_access_token(account_id, client_id, client_secret)

  # Function-specific API stuff
  api_url <- generate_url(query = "listmeetings", user_id = user_id)

  elements <- list()

  next_token <- ""
  while (next_token != "STOP") {
    resp <- zoom_api_request(
      verb = "GET",
      url = api_url,
      token = access_token,
      query_params = list(
        page_size = page_size,
        next_page_token = next_token,
        type = meeting_type
      )
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
    parsed <- jsonlite::fromJSON(.x, flatten = TRUE)

    # Handle empty meetings response
    if (is.null(parsed$meetings) || length(parsed$meetings) == 0) {
      return(data.frame())
    }

    df <- as.data.frame(parsed, stringsAsFactors = FALSE) %>%
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::everything(),
        as.character
      ))
    return(df)
  }

  df <- purrr::map_dfr(elements, list_to_df) %>%
    janitor::clean_names()

  # Remove pagination columns only if they exist
  pagination_cols <- c("page_size", "next_page_token", "total_records")
  existing_pagination_cols <- intersect(pagination_cols, names(df))

  if (length(existing_pagination_cols) > 0) {
    df <- df %>% dplyr::select(-all_of(existing_pagination_cols))
  }

  # Add meeting format labels (only if data and meetings_type column exist)
  if (nrow(df) > 0 && "meetings_type" %in% names(df)) {
    df$meeting_format_label <- meeting_format_labels[df$meetings_type]
    # Set NA for any unknown meeting types
    df$meeting_format_label[is.na(df$meeting_format_label)] <- "Unknown"
  }

  return(df)
}
