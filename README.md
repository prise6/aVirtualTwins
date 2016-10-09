# aVirtualTwins

An adaptation of VirtualTwins method from [Foster, J. C., Taylor, J. M.G. and Ruberg, S. J. (2011)](http://onlinelibrary.wiley.com/doi/10.1002/sim.4322/abstract)

VirtualTwins is a method of subgroup identification from randomized clinical trial data.

In 2015, as an intern in a french pharmaceutical group, i worked on this method and develop a package based on Jared Foster and al method.

## (Very) Quick Preview

```r
# Load data
data(sepsis)
# Format data
vt.obj <- vt.data(dataset         = sepsis,
                  outcome.field   = "survival",
                  treatment.field = "THERAPY",
                  interactions    = TRUE)
# Print Incidences of sepsis data
vt.obj$getIncidences()
# $table
#            trt
# resp        0    1     sum  
#   0         101  188   289  
#   1         52   129   181  
#   sum       153  317   470  
#   Incidence 0.34 0.407 0.385
#
# $rr
# [1] 1.197059
#
# First step : create random forest model
vt.for <- vt.forest(forest.type  = "one",
                    vt.data      = vt.obj,
                    interactions = TRUE,
                    ntree        = 500)
# Second step : find rules in data 
vt.trees <- vt.tree(tree.type = "class",
                    vt.difft  = vt.for, 
                    threshold = quantile(vt.for$difft, seq(.5,.8,.1)),
                    maxdepth  = 2)
# Print results
vt.sbgrps <- vt.subgroups(vt.trees)
knitr::kable(vt.sbgrps)
```
|      |Subgroup                    |Subgroup size |Treatement event rate |Control event rate |Treatment sample size |Control sample size | RR (resub)| RR (snd)|
|:-----|:---------------------------|:-------------|:---------------------|:------------------|:---------------------|:-------------------|----------:|--------:|
|tree1 |PRAPACHE>=26.5              |157           |0.752                 |0.327              |105                   |52                  |      2.300|    1.856|
|tree3 |PRAPACHE>=26.5 & AGE>=51.74 |120           |0.897                 |0.31               |78                    |42                  |      2.894|    1.991|



## Infos 

Currently this package works for RCT with two treatments groups and binary outcome.

Most of the package use Reference Class programing (in R). Feel free to create your own classes.

Of course, subgroup identification in general with two treatment and severals group can be possible.

## Help & Documentation

``` r
vignette("full-example", package = "aVirtualTwins")
```

Here's a link to my intern dissertation (french version) [La recher de sous-groupes par Virtual Twins](http://upload.timfaitsoncinema.fr/p/2016-09/57e6a8ff.pdf) (parts V & VI).


## Install

``` r
# use devtools library
library(devtools)
# install from github
devtools::install_github("prise6/aVirtualTwins", build_vignettes = TRUE)
# load library
library(aVirtualTwins)
```


## To-do list

* Link to my simulation
* Submit to CRAN
* Use R6 for perfs issues
* Vignette on-line


## News

See NEWS file


## Contact

vieille.francois _at_ gmail.com


