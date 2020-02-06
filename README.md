# metaRmat

[![R build status](https://github.com/lebebr01/metaRmat/workflows/R-CMD-check/badge.svg)](https://github.com/lebebr01/metaRmat/actions?workflow=R-CMD-check)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/lebebr01/metaRmat?branch=master&svg=true)](https://ci.appveyor.com/project/lebebr01/metaRmat)


## Package Installation

The package can be installed directly from GitHub using devtools or the remotes package with the following command:


```r
remotes::install_github("lebebr01/metaRmat")
```

To ensure that the vignettes are build, add the `build_vignettes` argument:


```r
remotes::install_github("lebebr01/metaRmat",
                        build_vignettes = TRUE)
```

## Explore vignettes
You can then explore the vignettes which give the basic functionality of the package. The two vignettes can be accessed with the following code:


```r
vignette(package = 'metaRmat')
```

