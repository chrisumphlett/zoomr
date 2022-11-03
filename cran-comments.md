## Initial Release

* Initial release to CRAN.
* Second submission, removed "with R!" from title and added \value tag to exported functions.


## Test environments

* Developed on and tested with Windows 11, R 4.1
* Tested on R-devel with devtools::check_win_devel()
* Testing against multiple Linux platforms with devtools::check_rhub()


## R CMD check results

* On `devtools::check()`: 0 errors √ | 0 warning x | 0 notes √
* On `devtools::check_rhub()` there is a NOTE I haven't seen before, and which others online are dismissing. This occur ONLY on Fedora Linux (R-hub): `checking HTML version of manual ... NOTE Skipping checking HTML validation: no command 'tidy' found.`


## No reverse dependencies
