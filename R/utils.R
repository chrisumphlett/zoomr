#' Checks responses against Zoom response codes and returns error message.
#'
#' @param res results object from httr
#' @keywords internal

zoom_response_codes <-
  function(res){
    
    # Exit if fine:
    if(res$status_code == 200){
      return()
    }
    
    #  Get error message
    error_message <-
      switch(
        as.character(res$status_code),
        `4700` =
          c("Zoom API reported an authentication error (4700):",
            "Your access token does not contain permission to access the",
            "requested API endpoint scopes."),
        `300` =
          c("Zoom API reported an invalid request (300):",
            "The next page token is invalid or expired."),
        `400` =
          c("Zoom API reported an invalid request:",
            "Bad Request."),
        `3001` =
          c("Zoom API reported an invalid request (3001):",
            "Meeting/Webinar ID does not exist"),
        `401` =
          c("Zoom API reported an invalid access token (401)"),
        `404` =
          c("Zoom API reported webinar Id is not found or has expired."),
        # Default response for unknown status code:
        c(glue::glue("Zoom API reported an atypical status code {res$status_code}"),
          glue::glue("Full response: {res}"),
          "A dictionary of status codes can be found here: https://developer.mozilla.org/en-US/docs/Web/HTTP/Status",
          "Please check your request, and report at https://github.com/chrisumphlett/zoomr/issues if reoccurring:")
      )
    
    # Report the error message:
    rlang::abort(error_message)
    
  }


#' Get Access token
#' 
#' Request a new token each time user executes a package function.
#' 
#' @param account_id Zoom API app Account Id.
#' @param client_id Zoom API app Client Id.
#' @param client_secret Zoom API app Client Secret.
#' 
#' @import httr 
#'
#' @keywords internal

get_access_token <-
  function(account_id, client_id, client_secret) {

    url <- paste0("https://zoom.us/oauth/token?grant_type=account_credentials&account_id=", account_id)
    
    request <- httr::POST(
      url = url,
      httr::authenticate(client_id, client_secret))
    
    # Check if response type is OK
    zoom_response_codes(request)
    
    token <- httr::content(request)$access_token
    
    return(token)
  }

#' Generate URL for specific API query by type and (if appropriate) ID
#' 
#' This is based on the function of the same name in the qualtRics package.
#'
#' @param query string.  The specific API query desired.  Generally named the
#'   same as associated functions but without underscores, so the request for
#'   `getwebinardetails()` would be be "getwebinardetails".
#' @param ... Named elements of URL for specific query desired, such as
#'   `webinar_id`.
#'
#' @importFrom glue glue
#'
#' @return Endpoint URL to be passed to querying tools
#' @keywords internal

generate_url <-
  function(query, ...){
    
    args <- list(...)
    list2env(args, envir = environment())
    
    # Get the user's specific base URL from environment
    # (and check it again in case the user has modified it externally somehow):
    base_url <- "api.zoom.us"

    # Construct URL root for the v3 api endpoint:
    root_url <-
      glue::glue("https://{base_url}/v2")
    
    # List of templates for how to build URLs
    # (add to this when new functions made):
    endpoint_template <-
      switch(
        query,
        getwebinardetails = "{rooturl}/report/webinars/{webinar_id}",
        getwebinarparticipants = "{rooturl}/past_webinars/{webinar_id}/participants",
        getwebinarqanda = "{rooturl}/report/webinars/{webinar_id}/qa",
        getwebinarpolls = "{rooturl}/report/webinars/{webinar_id}/polls",
        listwebinars = "{rooturl}/users/{user_id}/webinars",
        getwebinarregistrants = "{rooturl}/webinars/{webinar_id}/registrants",
        getpanelists = "{rooturl}/webinars/{webinar_id}/panelists",
        getusers = "{rooturl}/users",
        gettrackingsources = "{rooturl}/webinars/{webinar_id}/tracking_sources",
        listmeetings = "{rooturl}/users/{user_id}/meetings",
        getmeetingdetails = "{rooturl}/report/meetings/{meeting_id}",
        getmeetingparticipants = "{rooturl}/report/meetings/{meeting_id}/participants",
        rlang::abort("Internal error: invalid URL generation query")
      )

    # Construct the actual URL:
    api_url <- glue::glue(endpoint_template, rooturl = root_url, ...)
  }


#' Generate Query Parameters for specific API query
#' 
#' This is based on the `generate_url()` function.
#'
#' @param query string.  The specific API query desired.  Generally named the
#'   same as associated functions but without underscores, so the request for
#'   `getwebinardetails()` would be be "getwebinardetails".
#' @param ... Named query parameter elements for desired api endpoint, such as
#'   `next_page_token`.
#'
#' @importFrom glue glue
#'
#' @return Query parameters to be passed to querying tools
#' @keywords internal

generate_query_params <-
  function(query, ...){
    
    args <- list(...)
    list2env(args, envir = environment())
    
    # List of templates for how to build URLs
    # (add to this when new functions made):
    params_template <-
      switch(
        query,
        getwebinarparticipants = "list(page_size = 300)",
        rlang::abort("Internal error: invalid query parameters generation")
      )
    
    # Construct the actual URL:
    api_query_params <- glue::glue(params_template, ...)
  }

# query = list(page_size = 2,
#              next_page_token = "AsM0AJYGmwMWdMc5YoOLX5CvMUts8D1l1A2"))

#' Send httr requests to Zoom API
#' 
#' This is based on the function of the same name in the qualtRics package.
#'
#' @param verb Type of request to be sent (@seealso [httr::VERB()])
#' @param url Zoom endpoint URL created by [generate_url()] functions
#' @param ... arguments passed to httr::content when parsing
#' 
#' @import httr
#' 
#' @keywords internal

zoom_api_request <-
  function(verb = c("GET", "POST"),
           url = url,
           body = NULL,
           as = c("parsed", "raw"),
           token,
           query_params = query_params,
           ...
  ) {
    # Match args
    verb <- rlang::arg_match(verb)

    # Send request to Zoom API
    res <- httr::RETRY(
      verb,
      url = url,
      httr::add_headers(
        "Authorization" = paste0("Bearer ", token)
      ),
      # body = body,
      times = 3,
      terminate_on = 400:451,
      quiet = TRUE,
      query = query_params
    )
    # Check if response type is OK
    zoom_response_codes(res)

    # Get content out:
    cnt <-
      httr::content(
        x = res,
        "text",
        ...
      )

    # return(cnt)
    return(res)
  }