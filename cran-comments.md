I received an email that I need to fix the note on CRAN by 8/27/24.
Apologies for missing that deadline.

I tried to submit on 9/8 but was rejected for failing when Suggests packages
aren't available. I have either moved packages to Depends, or added
requireNamespace around functions from the packages in Suggests. I reran the
tests and everything passes.

## Test environments
* local Windows 10 install, R 4.4.1
* win-builder (devel)
* mac-builder
* macOS 14.6.1 (GitHub Actions), R 4.4.1
* Ubuntu 22.04.4 LTS (devel, GitHub Actions)
* Ubuntu 22.04.4 LTS (oldrel-1, GitHub Actions)
* Ubuntu 22.04.4 LTS (latest, GitHub Actions)
* Microsoft Windows Server 2022 (release, GitHub Actions)
* Rhub, linux (R-devel)
* Rhub, macos (R-devel)
* Rhub, windows (R-devel)


## R CMD check results


* local Windows 10 install (9/8/24): 0 errors | 0 warnings | 0 notes

* win-builder (devel) (9/8/24): 1 NOTE (new submission)

* mac-builder (9/8/24): OK

* macOS 14.6.1 (GitHub Actions) (9/8/24): OK

* Ubuntu 22.04.4 LTS (devel, GitHub Actions) (9/8/24): OK

* Ubuntu 22.04.4 LTS (oldrel-1, GitHub Actions) (9/8/24): OK

* Ubuntu 22.04.4 LTS (latest, GitHub Actions) (9/8/24): OK

* Microsoft Windows Server 2022 (release, GitHub Actions) (9/8/24): OK

* Rhub, linux (R-devel) (9/8/24): OK

* Rhub, macos (R-devel) (9/8/24): OK

* Rhub, windows (R-devel) (9/8/24): OK



## Reverse dependencies

My other packages on CRAN are failing because this was removed from CRAN.
There are no real changes to this package, so it won't affect the reverse
dependencies.
