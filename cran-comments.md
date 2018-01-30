This is the second submission

----------------------------------------------------------------

## v1.0.1 : patch to cran warning:

>CRAN packages using undeclared packages in vignettes

`rmarkdown` seems to be the issue. Added `rmarkdown` to suggest field.
`e1071` was added too.

## Test environments

* Linux, Debian jessie, R 3.4.3
* win-builder (devel and release)


## R CMD check result

Status: OK

R CMD check results
0 errors | 0 warnings | 0 notes







This is the first submission

----------------------------------------------------------------

## Test environments

* Linux, Debian jessie, R 3.2.5
* win-builder (devel and release)


## R CMD check result

There were no ERRORs or WARNINGs.

Only one NOTE:

[french]
* VT.difft: possible error in new(structure("VT.difft", package = "aVirtualTwins"), ...): ... utilisé dans une situation où il n'existe pas

[english]
* VT.difft: possible error in new(structure("VT.difft", package = "aVirtualTwins"), ...): ... used in a situation where it does not exist

It seems to be a temporary bug in R-devel [ref](http://r.789695.n4.nabble.com/R-CMD-check-quot-quot-used-in-a-situation-where-it-does-not-exist-td4701779.html)

This NOTE doesn't exist in stable version of R. No NOTEs for win-builder tests.

