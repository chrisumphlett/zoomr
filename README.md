# zoomr

Connect to your 'Zoom' reporting data

## Purpose

`zoom` facilitates making a connection to the Zoom Reporting API. Currently it is used for getting data on webinars. Many other API endpoints are available, this documentation is found at <https://marketplace.zoom.us/docs/api-reference/zoom-api/>.

## Installation

The development version can be installed from GitHub: `remotes::install_github("chrisumphlett/zoomr")`.

## Usage

A server-to-server app must be created within your Zoom account first. This will provide an Account Id, Client Id, and Client Secret. These are required for all of the api call functions in this package in order to generate an access token.

An expected workflow would start with looking up the user Id's for webinar hosts. This is done with the `get_account_users()` function. This will produce a data frame with user data. The user Id is then used in `list_webinars()` to find the webinars hosted by the user in the last 90 days. Once you have a webinar Id the rest of the exported api call functions can be used:

-   `get_webinar_details()`. More information on the webinar such as the topic, start and end time, duration, and number of participants.
-   `get_webinar_registrants()`. Data on who registered for a webinar.
-   `get_registration_questions()`. Their answers to questions you asked when they registered.
-   `get_webinar_participants()`. Data on who attended the webinar.
-   `get_panelists()`. Figure out which of your participants were actually panelists.
-   `get_webinar_polls()` and `get_webinar_qanda()`: Poll and Q&A data from the webinar. *Post-webinar survey data is not available in the Zoom API as of 2022-09-28.*
