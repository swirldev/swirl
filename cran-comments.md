## Release summary

This is the first attempted CRAN release of swirl 2.4.1.
This patch is in response to a message from Brian Ripely
informing me that a test fails in the Latin-1 locale, and a
message from Hadley Wickham informing me that the new
version of testthat introduces breaking changes to swirl.
This patch should resolve both of those issues.

## Test environments

* local OSX Yosemite install, R 3.2.4
* Ubuntu 12.04 (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs.