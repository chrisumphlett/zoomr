## zoomr 0.2.0

* BREAKING CHANGE: Due to changes Zoom made to the endpoint that was used for `get_webinar_participants()`, a different endpoint is now being used. This endpoint returns different fields, and the resulting data frame will have different columns. Switching was important in order to restore the ability to use the `registrant_id` for a participant to join it back to the registration data.


## Test environments

* Developed on and tested with Windows 11, R 4.1
* Tested on R-devel with devtools::check_win_devel()
* Testing against multiple Linux platforms with devtools::check_rhub()


## R CMD check results

* On `devtools::check()`: 0 errors √ | 0 warning x | 0 notes √


## No reverse dependencies
