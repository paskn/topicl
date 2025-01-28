
<!-- README.md is generated from README.Rmd. Please edit that file -->

# topicl

<!-- badges: start -->

<!-- badges: end -->

The goal of topicl is to provide tools for identifying stable
(reproducible) topics and for their analysis.

## Installation

You can install the development version of topicl from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("paskn/topicl")
```

## Example

See
[vignette](https://www.pashakhin.org/topicl/articles/find_stable_topics-jaccard.html)
for details.

This is a basic example which shows you how to solve a common problem:

``` r
library(topicl)
library(stm)
library(dplyr)

modA <- stm(poliblog5k.docs, 
            poliblog5k.voc, K=25,
            prevalence=~rating, 
            data=poliblog5k.meta,
            max.em.its=2, 
            init.type="Random",
            seed = 9934,
            verbose = F) 
           
modB <- stm(poliblog5k.docs, 
            poliblog5k.voc, K=25,
            prevalence=~rating, 
            data=poliblog5k.meta,
            max.em.its=2, 
            init.type="Random",
            seed = 9576,
            verbose = F) 
           
compare_solutions(list(modA, modB), depth=500) |> 
  arrange(desc(jaccard)) |> 
  head()
#> # A tibble: 6 × 5
#>   model_id_A topic_id_A model_id_B topic_id_B jaccard
#>   <chr>      <chr>      <chr>      <chr>        <dbl>
#> 1 mod_1      topic_4    mod_2      topic_24     0.161
#> 2 mod_1      topic_7    mod_2      topic_6      0.152
#> 3 mod_1      topic_4    mod_2      topic_3      0.144
#> 4 mod_1      topic_24   mod_2      topic_24     0.143
#> 5 mod_1      topic_3    mod_2      topic_5      0.142
#> 6 mod_1      topic_4    mod_2      topic_9      0.142
```
