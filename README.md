
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rando <img src='man/figures/logo.svg' align="right" height="139" />

<!-- badges: start -->

[![R build
status](https://github.com/MyKo101/rando/workflows/R-CMD-check/badge.svg)](https://github.com/MyKo101/rando/actions)
[![Codecov test
coverage](https://codecov.io/gh/MyKo101/rando/branch/master/graph/badge.svg)](https://codecov.io/gh/MyKo101/rando?branch=master)
<!-- badges: end -->

The goal of rando is to provide easier generating of random numbers in a
manner that is context aware, and reproducible.

## Installation

You can install the development version of rando from
[Github](https://github.com/MyKo101/rando) with:

    install.packages("remotes")
    install_github("MyKo101/rando")

Once installed, to load `rando`, use

    library(rando)

## Example

With rando, generating random numbers becomes incredibly easy, as we do
not need to define how many random numbers we need. `rando` will figure
out how many you need based on where the number generator is being used.

This works for `tibble()` declarations

``` r
df <- tibble(id = 1:10,
       x = r_norm())
df
#> # A tibble: 10 x 2
#>       id        x
#>    <int>    <dbl>
#>  1     1 -0.717  
#>  2     2  0.786  
#>  3     3 -0.751  
#>  4     4  0.195  
#>  5     5 -0.575  
#>  6     6 -0.708  
#>  7     7  0.00443
#>  8     8 -0.940  
#>  9     9  1.35   
#> 10    10 -0.684
```

and inside of `dplyr` verbs

``` r
mutate(df, y = r_unif())
#> # A tibble: 10 x 3
#>       id        x       y
#>    <int>    <dbl>   <dbl>
#>  1     1 -0.717   0.160  
#>  2     2  0.786   0.849  
#>  3     3 -0.751   0.248  
#>  4     4  0.195   0.290  
#>  5     5 -0.575   0.742  
#>  6     6 -0.708   0.466  
#>  7     7  0.00443 0.275  
#>  8     8 -0.940   0.987  
#>  9     9  1.35    0.754  
#> 10    10 -0.684   0.00774
```

Parameters can also be used to define the number of values to return. If
parameters are longer than 1, `rando` will try to return the same number
of random values, unless there is a clash between two of the parameters

``` r
r_norm(mean = 1:10)
#>  [1]  1.646460  1.426851  2.390095  3.919675  5.738230  7.006514  6.758490
#>  [8]  6.825054  9.793100 10.373449
r_norm(mean=1:10,sd=1:2)
#> Error: Inconsistent parameter lengths supplied to r_norm()
```

If you want to manually define the number of random numbers to be
generated, there are two ways to do it. The old fashioned way: providing
the `n` argument (this must be named)

``` r
r_unif(n=20)
#>  [1] 0.073736296 0.694316115 0.126838400 0.389872381 0.744243355 0.417200313
#>  [7] 0.983483450 0.977667520 0.069352079 0.566053953 0.003966586 0.500302361
#> [13] 0.548635787 0.657779980 0.770831601 0.171120996 0.838838250 0.879830294
#> [19] 0.620431531 0.858520843
```

Or, if we are generating many random numbers, we can set a default `n`
value to be used globally

``` r
set_n(15)
r_norm(mean=3)
#>  [1] 3.850911 1.233965 3.270736 2.466382 1.436930 3.596642 3.175772 3.177666
#>  [9] 2.649025 2.103756 3.075654 2.548388 5.053156 3.033173 3.193646
r_binom(size=3)
#>  [1] 2 3 3 0 1 2 3 1 0 1 1 2 1 3 1
```

## Safer and replicable

The `rando` functions also check if parameters being supplied are viable
and throws an informative error if not. This is different to the default
`stats` random number generating functions, which may return a lot of
`NaN` values with only a vague warning.

``` r
rnorm(n=10,sd=-1)
#> Warning in rnorm(n = 10, sd = -1): NAs produced
#>  [1] NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN
r_norm(sd=-1)
#> Error: sd provided to r_norm() must be strictly positive
```

All `rando` functions can also take a `.seed` argument which does one of
two things:

  - If a numeric is supplied, then `rando` will set this as the random
    seed before generating the values
  - If a TRUE is supplied, then `rando` will randomly generate a numeric
    value to be used.

If `.seed` is not `NULL` (the default), then this `seed` value (supplied
or generated) will be attached to the output, and can be extracted with
`pull_seed()`

This allows for greater replicability in results.

``` r
r_norm(.seed = 42)
#>  [1]  1.37095845 -0.56469817  0.36312841  0.63286260  0.40426832 -0.10612452
#>  [7]  1.51152200 -0.09465904  2.01842371 -0.06271410  1.30486965  2.28664539
#> [13] -1.38886070 -0.27878877 -0.13332134
#> attr(,"seed")
#> [1] 42
r_norm(.seed = 42)
#>  [1]  1.37095845 -0.56469817  0.36312841  0.63286260  0.40426832 -0.10612452
#>  [7]  1.51152200 -0.09465904  2.01842371 -0.06271410  1.30486965  2.28664539
#> [13] -1.38886070 -0.27878877 -0.13332134
#> attr(,"seed")
#> [1] 42

x <- r_norm(.seed=TRUE)
x
#>  [1] -1.0515017  2.8143380  1.1880200 -1.2010801 -1.1589546 -0.1876997
#>  [7] -0.1515049  0.7168907 -0.2086623 -1.0248107  0.7394365 -0.5944315
#> [13] -1.9588881  0.5869532  0.6124257
#> attr(,"seed")
#> [1] 1020465408

r_norm(.seed=pull_seed(x))
#>  [1] -1.0515017  2.8143380  1.1880200 -1.2010801 -1.1589546 -0.1876997
#>  [7] -0.1515049  0.7168907 -0.2086623 -1.0248107  0.7394365 -0.5944315
#> [13] -1.9588881  0.5869532  0.6124257
#> attr(,"seed")
#> [1] 1020465408
```

## Blueprints

In order to make simulations easier, `rando` provides the `blueprint()`
function. This function creates a plan for a simulated dataset using
`rando` functions.

``` r
make_tbl <- blueprint(
  x = r_norm(),
  y = r_norm()
)

make_tbl(n=2)
#> # A tibble: 2 x 2
#>       x     y
#>   <dbl> <dbl>
#> 1 -1.89 1.34 
#> 2 -2.28 0.913

make_tbl(n=5)
#> # A tibble: 5 x 2
#>        x      y
#>    <dbl>  <dbl>
#> 1  0.316 -0.154
#> 2  1.86   1.46 
#> 3 -0.396 -1.42 
#> 4 -1.08   0.481
#> 5  1.75   0.323
```

These blueprints can accept additional arguments and will be generated
based on these arguments

``` r
make_tbl2 <- blueprint(
  x = r_norm(mean=x_mu),
  y = r_unif(min=y_min,max=y_max)
)

set_n(10000)
make_tbl2(x_mu = 10, y_min = -10, y_max=-5) %>%
  summarise(n = n(), mean_x = mean(x), min_y = min(y), max_y = max(y))
#> # A tibble: 1 x 4
#>       n mean_x min_y max_y
#>   <int>  <dbl> <dbl> <dbl>
#> 1 10000   10.0 -10.0 -5.00
```

This then allows for quick generation of simulation data using `pmap()`
and analysis using `map()`

``` r
make_sim <- blueprint(
  x = r_norm(mean = x_mu),
  y = r_norm(mean = 2*x+10, sd = 2)
)

tibble(x_mu = r_unif(n = 5, -10, 10)) %>%
  pmap(make_sim, n = 100) %>%
  map(lm, formula = y ~ x) %>%
  map_dfr(broom::tidy)
#> # A tibble: 10 x 5
#>    term        estimate std.error statistic  p.value
#>    <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#>  1 (Intercept)     9.29     1.35       6.89 5.45e-10
#>  2 x               1.92     0.202      9.48 1.60e-15
#>  3 (Intercept)     8.69     0.723     12.0  5.58e-21
#>  4 x               2.38     0.193     12.3  1.32e-21
#>  5 (Intercept)    10.6      0.726     14.6  2.91e-26
#>  6 x               1.82     0.252      7.20 1.22e-10
#>  7 (Intercept)    10.1      0.770     13.1  3.20e-23
#>  8 x               2.06     0.202     10.2  4.72e-17
#>  9 (Intercept)     9.78     0.426     22.9  3.54e-41
#> 10 x               1.68     0.218      7.72 1.02e-11
```

## Distribution Functions

The majority of random number generating functions from the `stats`
package have been translated into `rando` functions. Be sure to look
into the documentation for the `rando` functions you use, as some have
re-parametrised. Functions names for transitioning from `stats` to
`rando` generally follow the same naming convention, that is `r*()`
becomes `r_*()`, e.g. `r_norm()` replaces `rnorm()`. The only exceptions
are `r_tdist()` and `r_fdist()` to take over the roles of `rt()` and
`rf()`, respectively. `rando` also includes several new distributions
such as `r_bern()` and `r_letters()`.

## Arbitrary Distributions

The `r_cdf()` function is a dynamic random number generator. It can take
any cdf as an argument and produce random numbers with the associated
distribution.

``` r

my_fun <- function(x,beta=1){
  if_else(x < 0, 0, 1-exp(-beta*x))
}

set_n(1000)
x_data <- r_cdf(my_fun)

hist(x_data,breaks=seq(0,10,0.1))
```

<img src="man/figures/README-r_cdf-1.png" width="100%" /> Any additional
arguments used by the function, can be passed to `r_cdf()`, and will be
used in determining the number of values to generate (just as in the
other distribution functions above)

``` r
r_cdf(my_fun,beta=1:10)
#>  [1] 1.59363151 0.01710057 0.51777959 0.10563731 0.15656352 0.04890561
#>  [7] 0.05313754 0.10311007 0.01916289 0.09977221
```

Finally, `purrr`-style functions can be used for `r_cdf()` to allow for
even briefer function definitions. These have been extended to allow for
the use of additional named arguments to be passed to these `<lambda>`
functions. Either `.x` or `.t` can be used for the random variable.

``` r
set_n(20)
r_cdf(~1-exp(-.x),min=0)
#>  [1] 1.00280643 0.51202178 3.15050483 0.38757920 0.16273856 1.37652755
#>  [7] 0.41813254 1.14622712 1.26543641 0.01011491 0.65036416 1.35177970
#> [13] 1.25859380 0.30105710 1.45331025 0.22260547 1.71133876 0.12983680
#> [19] 0.41169524 0.26691556

r_cdf(~1-exp(-beta*.x),beta=1:10,min=0,n=10)
#>  [1] 0.892275572 0.172501802 0.160342455 0.432735682 0.299936533 0.004011393
#>  [7] 0.133234262 0.150531530 0.004047155 0.426167250
```

## Code of Conduct

Please note that the rando project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
