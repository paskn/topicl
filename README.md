
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
#> [1] "Done"
#>   model_id_1 model_id_2 topic_id_1 topic_id_2   jaccard
#> 1      mod_1      mod_2    topic_4   topic_24 0.1614402
#> 2      mod_1      mod_2    topic_4   topic_25 0.1614402
#> 3      mod_1      mod_2    topic_5    topic_1 0.1614402
#> 4      mod_1      mod_2    topic_5    topic_2 0.1614402
#> 5      mod_1      mod_2    topic_5    topic_3 0.1614402
#> 6      mod_1      mod_2    topic_5    topic_4 0.1614402
```
