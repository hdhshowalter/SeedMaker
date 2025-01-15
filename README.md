
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SeedMaker

<!-- badges: start -->

[![R-CMD-check](https://github.com/hdhshowalter/SeedMaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hdhshowalter/SeedMaker/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/hdhshowalter/SeedMaker/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/hdhshowalter/SeedMaker/actions/workflows/test-coverage.yaml)
[![lint](https://github.com/hdhshowalter/SeedMaker/actions/workflows/lint.yaml/badge.svg)](https://github.com/hdhshowalter/SeedMaker/actions/workflows/lint.yaml)
<!-- badges: end -->

{SeedMaker} is a mechanism for easily generating and organizing a
collection of seeds from a single seed, which may be subsequently used
to ensure reproducibility in processes/pipelines that utilize multiple
random components (e.g., trial simulation).

## Installation

Install the released version of {SeedMaker} from CRAN:

``` r
install.packages("SeedMaker")
```

Or install the development version of {SeedMaker} from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("hdhshowalter/SeedMaker")
```

## Examples

The following calls are representative of the most common ways to engage
`seed_maker()`:

``` r
library(SeedMaker)

#simple call
seed_maker(
  seed = 1234,
  level_names = "sim",
  n_per_level = 5
)
#> $sim1
#> [1] 761680
#> 
#> $sim2
#> [1] 630678
#> 
#> $sim3
#> [1] 304108
#> 
#> $sim4
#> [1] 932745
#> 
#> $sim5
#> [1] 295846

#slightly more complex call
seed_maker(
  seed = 1234,
  level_names = c("sim", "replicate"),
  n_per_level = c(5, 10)
)
#> $sim1
#> $sim1$replicate1
#> [1] 241906
#> 
#> $sim1$replicate2
#> [1] 928350
#> 
#> $sim1$replicate3
#> [1] 847527
#> 
#> $sim1$replicate4
#> [1] 251548
#> 
#> $sim1$replicate5
#> [1] 379402
#> 
#> $sim1$replicate6
#> [1] 894769
#> 
#> $sim1$replicate7
#> [1] 223940
#> 
#> $sim1$replicate8
#> [1] 325385
#> 
#> $sim1$replicate9
#> [1] 766829
#> 
#> $sim1$replicate10
#> [1] 904458
#> 
#> 
#> $sim2
#> $sim2$replicate1
#> [1] 548592
#> 
#> $sim2$replicate2
#> [1] 681521
#> 
#> $sim2$replicate3
#> [1] 273200
#> 
#> $sim2$replicate4
#> [1] 819616
#> 
#> $sim2$replicate5
#> [1] 493964
#> 
#> $sim2$replicate6
#> [1] 997086
#> 
#> $sim2$replicate7
#> [1] 271560
#> 
#> $sim2$replicate8
#> [1] 284789
#> 
#> $sim2$replicate9
#> [1] 676748
#> 
#> $sim2$replicate10
#> [1] 251192
#> 
#> 
#> $sim3
#> $sim3$replicate1
#> [1] 242055
#> 
#> $sim3$replicate2
#> [1] 599300
#> 
#> $sim3$replicate3
#> [1] 364599
#> 
#> $sim3$replicate4
#> [1] 82239
#> 
#> $sim3$replicate5
#> [1] 978159
#> 
#> $sim3$replicate6
#> [1] 606003
#> 
#> $sim3$replicate7
#> [1] 208137
#> 
#> $sim3$replicate8
#> [1] 315461
#> 
#> $sim3$replicate9
#> [1] 186198
#> 
#> $sim3$replicate10
#> [1] 549779
#> 
#> 
#> $sim4
#> $sim4$replicate1
#> [1] 542579
#> 
#> $sim4$replicate2
#> [1] 802509
#> 
#> $sim4$replicate3
#> [1] 411648
#> 
#> $sim4$replicate4
#> [1] 945114
#> 
#> $sim4$replicate5
#> [1] 842371
#> 
#> $sim4$replicate6
#> [1] 967436
#> 
#> $sim4$replicate7
#> [1] 686936
#> 
#> $sim4$replicate8
#> [1] 386971
#> 
#> $sim4$replicate9
#> [1] 866601
#> 
#> $sim4$replicate10
#> [1] 409145
#> 
#> 
#> $sim5
#> $sim5$replicate1
#> [1] 707912
#> 
#> $sim5$replicate2
#> [1] 981104
#> 
#> $sim5$replicate3
#> [1] 257273
#> 
#> $sim5$replicate4
#> [1] 183803
#> 
#> $sim5$replicate5
#> [1] 164537
#> 
#> $sim5$replicate6
#> [1] 532250
#> 
#> $sim5$replicate7
#> [1] 622868
#> 
#> $sim5$replicate8
#> [1] 176299
#> 
#> $sim5$replicate9
#> [1] 709384
#> 
#> $sim5$replicate10
#> [1] 166772

#slightly more complex call with non-indexed names at level 2
seed_maker(
  seed = 1234,
  level_names = list("sim", c("component_a", "component_b", "component_c")),
  n_per_level = c(5, 3)
)
#> $sim1
#> $sim1$component_a
#> [1] 241906
#> 
#> $sim1$component_b
#> [1] 928350
#> 
#> $sim1$component_c
#> [1] 847527
#> 
#> 
#> $sim2
#> $sim2$component_a
#> [1] 548592
#> 
#> $sim2$component_b
#> [1] 681521
#> 
#> $sim2$component_c
#> [1] 273200
#> 
#> 
#> $sim3
#> $sim3$component_a
#> [1] 242055
#> 
#> $sim3$component_b
#> [1] 599300
#> 
#> $sim3$component_c
#> [1] 364599
#> 
#> 
#> $sim4
#> $sim4$component_a
#> [1] 542579
#> 
#> $sim4$component_b
#> [1] 802509
#> 
#> $sim4$component_c
#> [1] 411648
#> 
#> 
#> $sim5
#> $sim5$component_a
#> [1] 707912
#> 
#> $sim5$component_b
#> [1] 981104
#> 
#> $sim5$component_c
#> [1] 257273

#simple call that generates seed values between 1 and 100 instead of between
#1 and 1000000
seed_maker(
  seed = 1234,
  level_names = "sim",
  n_per_level = 5,
  sample_vector = list(1:100)
)
#> $sim1
#> [1] 28
#> 
#> $sim2
#> [1] 80
#> 
#> $sim3
#> [1] 22
#> 
#> $sim4
#> [1] 9
#> 
#> $sim5
#> [1] 5

#simple call that returns named vector instead of list
seed_maker(
  seed = 1234,
  level_names = "sim",
  n_per_level = 5,
  flatten = TRUE
)
#>   sim1   sim2   sim3   sim4   sim5 
#> 761680 630678 304108 932745 295846
```
