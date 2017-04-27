I submitted it last night but failed the auto-test.
It said it couldn't fetch the images from the README.
I had not had this error on any of the four systems
I checked it on before submitting.
I changed it so those image files are no long in the 
.Rbuildignore (not sure why they were in the first place).
I moved them to the tools folder.
This seems like it should fix the problem.

## Test environments
* local Windows 7 install, R 3.4.0
* x86_64-pc-linux-gnu, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.3
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
