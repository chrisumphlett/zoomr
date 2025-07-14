# zoomr 0.4.0.9000

* Enhanced `get_meeting_participants()` with recurring meeting support:
* Added `include_all_instances` parameter to get participants from all occurrences
* Added `instance_date` column to identify specific meeting occurrences
* Standardized column names between different API endpoints

# zoomr 0.3.1

* Added support for regular Zoom meetings (in addition to webinars)
* Added `list_meetings()` to retrieve meetings for a user
* Added `get_meeting_participants()` to get participant data from meetings  
* Added `get_meeting_details()` to get meeting metadata and statistics

# zoomr 0.3.0.9000

* development version

# zoomr 0.3.0

* Added `get_tracking_sources()` to retrieve visitor and registration count by tracking source for a webinar. 

# zoomr 0.2.0

* BREAKING CHANGE: Due to changes Zoom made to the endpoint that was used for `get_webinar_participants()`, a different endpoint is now being used. This endpoint returns different fields, and the resulting data frame will have different columns. Switching was important in order to restore the ability to use the `registrant_id` for a participant to join it back to the registration data.

# zoomr 0.1.1

* Added status query parameter to `get_webinar_registrants()`. From what I can tell the Zoom API endpoint is not set up correctly to retrieve registrants who cancel. 'Cancelled' is not one of the status values that is available. Neither 'pending' nor 'denied' return the cancelled registrants. Cancelled registrants are available via the manual exports in Zoom.
* Added status query parameter to `get_registration_questions()`.

# zoomr 0.1.0

* Publishing to CRAN. Performed final re-checks and cleanup.


# zoomr 0.0.3

* Handle API errors for the registrants endpoint when a webinar does not have any registrants. Both `get_registration_questions()` and `get_webinar_registrants()` will print a message to the console if there are no registrants but the webinar Id is found.


# zoomr 0.0.2

* `get_registration_questions()` updated for the case where a webinar does not have any registration question responses.
* Added a lot of `@import` and `@importFrom` comments to the Roxygen2 documentation.
* Removed some unneeded, commented out code.
* Updated some examples that referred to the wrong functions.


# zoomr 0.0.1

* First version. The following functions created, plus non-exported functions to support them.
* `get_account_users()`
* `list_webinars()`
* `get_webinar_details()`
* `get_webinar_registrants()`
* `get_registration_questions()`
* `get_webinar_participants()`
* `get_panelists()`
* `get_webinar_polls()`
* `get_webinar_qanda()`