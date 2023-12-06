# Campus Living Lab Green Stormwater Infrastructure Dashboard

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

<!-- About this dashboard.  To be filled out by Malcolm and Vanessa -->

## Contributing

To contribute to this project, please create a new branch for your changes and make a pull request. One easy way to do this from within R is with the `usethis` package and the `pr_*` functions. `pr_init("branch-name")` begins a new branch locally, `pr_push()` helps you create a new pull request, and after it is merged you can use `pr_finish()` to clean things up. More about this workflow [here](https://usethis.r-lib.org/articles/pr-functions.html).

### `renv`

This project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) for package management. When opening this repo as an RStudio Project for the first time, `renv` should automatically install itself and prompt you to run `renv::restore()` to install all package dependencies.

### Shiny

<!-- where to go to learn about Shiny and `bslib` -->

<!-- app structure including that all the plots are made with custom functions in R/ -->

#### Modifying plot theme

The theme for the `ggplot2` plots is controlled by `R/0-theme_gsi.R`.
(the filename starts with '0' because the files are sourced alphabetically and the theme is used by all the other functions in `R/`).

### Deployment
<!-- aspirational:
This app is published to \<URL\> automatically by a GitHub action whenever changes are made to the main branch of this repository.
If changes are not being reflected on the deployed app or there are other issues with the live app, please contact \@cct-datascience.
-->