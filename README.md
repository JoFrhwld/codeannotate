
<!-- README.md is generated from README.Rmd. Please edit that file -->

# codeannotate

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoFrhwld/codeannotate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoFrhwld/codeannotate/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The goal of codeannotate is to provide a quick way to add or remove
quarto code annotation markup to a code chunk.

## Installation

You can install the development version of codeannotate from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("JoFrhwld/codeannotate")
```

## Example

The quarto documentation provides the following example of code to
annotate.

``` r
library(tidyverse)
library(palmerpenguins)
penguins |>
  mutate(
    bill_ratio = bill_depth_mm / bill_length_mm,
    bill_area  = bill_depth_mm * bill_length_mm
  )
```

By selecting some lines of code, and triggering the `codeannotate`
addin, code annotation markup will be added to the code chunk. By
selecting some lines of code and triggering the `remove_codeannotate`
addin, the markup will be removed.

![](assets/code_annotate.gif)
