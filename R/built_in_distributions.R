#' @name r_norm
#'
#' @title Generate Normally Distributed Values
#'
#' @description
#' Generates a set of Normally distributed values.
#'
#' @param mean
#' vector of means
#'
#' @param sd
#' vector of standard deviations, strictly positive
#'
#' @param n
#' number of observations to generate. The [default_n()] function will
#' provide a default value within context
#'
#' @param .seed
#' One of the following:
#'
#' * NULL (default) will not change the current seed. This is the
#' usual case for generating random numbers.
#'
#' * A numeric value. This will be used to set the seed before generating
#' the random numbers. This seed will be stored with the results.
#'
#' * TRUE. A random seed value will be generated and set as the seed
#'  before the results are generated. Again, this will be stored with
#'  the results.
#'
#' To extract the random seed from a previously generated set of
#' values, use `pull_seed()`
#'
#'
#' @param ...
#' Unused
#'
#' @examples
#'
#' set_n(5)
#'
#' r_norm(10)
#'
#' r_norm(10,2)
#'
#' r_norm(1:10)
#'
#' r_norm(-2,n=10)
#'
#'
#' @export
r_norm <- function(mean=0,sd=1,...,n=default_n(mean,sd),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(sd)
  with_seed(
    .seed,
    stats::rnorm(n=n,mean=mean,sd=sd)
    )
}


#' @name r_beta
#'
#' @title Generate Beta Distributed Values
#'
#' @description
#' Generates a set of Beta distributed values.
#'
#' @param alpha,beta
#' vectors of shape parameters, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_beta(1,1)
#'
#' r_beta(1:10,2)
#'
#' r_beta(1,2,n=10)
#'
#'
#' @export
r_beta <- function(alpha,beta,...,n=default_n(alpha,beta),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(alpha)
  check_must_be_strictly_positive(beta)
  with_seed(
    .seed,
    stats::rbeta(n=n,shape1=alpha,shape2=beta)
  )
}


#' @name r_binom
#'
#' @title Generate Binomial Distributed Values
#'
#' @description
#' Generates a set of Binomial distributed values.
#'
#' @param size
#' vector of number of trials, positive integer
#'
#' @param prob
#' vector of probabilities of success on each trial, between 0 & 1
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_binom(10)
#'
#' r_binom(1:10)
#'
#' r_binom(10,0.2)
#'
#' r_binom(1,0.2,n=10)
#'
#'
#' @export
r_binom <- function(size,prob=0.5,...,n=default_n(size,prob),.seed=NULL){
  check_n(n)
  check_must_be_between(prob,0,1)
  check_must_be_integer(size)
  check_must_be_positive(size)
  with_seed(
    .seed,
    stats::rbinom(n = n,size = size,prob = prob)
  )
}


#' @name r_cauchy
#'
#' @title Generate Cauchy Distributed Values
#'
#' @description
#' Generates a set of Cauchy distributed values.
#'
#' @param location
#' vector of locations
#'
#' @param scale
#' vector of scales, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_cauchy(10)
#'
#' r_cauchy(1:10)
#'
#' r_cauchy(10,2)
#'
#' r_cauchy(10,2,n=10)
#'
#'
#' @export
r_cauchy <- function(location = 0, scale = 1,...,n=default_n(location,scale),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(scale)
  with_seed(
    .seed,
    stats::rcauchy(n = n,location = location, scale = scale)
  )
}


#' @name r_chisq
#'
#' @title Generate Chi-Squared Distributed Values
#'
#' @description
#' Generates a set of Chi-Squared distributed values.
#'
#' @param df
#' degrees of freedom, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_chisq(10)
#'
#' r_chisq(1:10)
#'
#' r_chisq(10,n=10)
#'
#'
#' @export
r_chisq <- function(df,...,n=default_n(df),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(df)
  with_seed(
    .seed,
    stats::rchisq(n=n,df=df)
  )
}


#' @name r_exp
#'
#' @title Generate Exponentially Distributed Values
#'
#' @description
#' Generates a set of Exponentially distributed values.
#'
#' @param rate
#' vector of rates, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_exp(10)
#'
#' r_exp(1:10)
#'
#' r_exp(10,n=10)
#'
#'
#' @export
r_exp <- function(rate = 1,...,n=default_n(rate),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(rate)
  with_seed(
    .seed,
    stats::rexp(n=n,rate = rate)
  )
}


#' @name r_fdist
#'
#' @title Generate F Distributed Values
#'
#' @description
#' Generates a set of F distributed values.
#'
#' @param df1,df2
#' vectors of degrees of freedom, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_fdist(1,1)
#'
#' r_fdist(1:10,2)
#'
#' r_fdist(10,2)
#'
#' r_fdist(10,2,n=10)
#'
#'
#' @export
r_fdist <- function(df1,df2,...,n=default_n(df1,df2),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(df1)
  check_must_be_strictly_positive(df2)
  with_seed(
    .seed,
    stats::rf(n=n,df1=df1,df2=df2)
  )
}


#' @name r_gamma
#'
#' @title Generate Gamma Distributed Values
#'
#' @description
#' Generates a set of Gamma distributed values. Can be defined by
#' one and only one of `scale`, `rate` or `mean.`
#' This _must_ be named in the call.
#'
#' @param shape
#' vector of shape parameters, strictly positive
#'
#' @param scale
#' vector of scale parameters, cannot be specified with `rate` and `mean`,
#' strictly positive
#'
#' @param rate
#' vector of rate parameters, cannot be specified with `scale` and `mean`,
#' strictly positive
#'
#' @param mean
#' vector of mean parameters, cannot be specified with `scale` and `rate`,
#' strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_gamma(10)
#'
#' r_gamma(1:10,scale=2)
#' r_gamma(1:10,rate=1/2)
#' r_gamma(1:10,mean=5)
#'
#' r_gamma(10,n=10)
#'
#'
#' @export
r_gamma <- function(shape,...,scale=1,rate=NULL,mean=NULL,
                    n=default_n(shape,scale,rate,mean),.seed=NULL){
  check_n(n)
  scale_provided <- !missing(scale)
  mean_provided <- !missing(mean)
  rate_provided <- !missing(rate)

  if(sum(scale_provided,mean_provided,rate_provided)>1)
    error_glue("Only one of scale, rate or mean can be provided to r_gamma()")

  if(rate_provided) scale <- 1/rate
  if(mean_provided) scale <- mean/shape

  check_must_be_strictly_positive(shape)
  if(scale_provided) check_must_be_strictly_positive(scale)
  if(mean_provided) check_must_be_strictly_positive(mean)
  if(rate_provided) check_must_be_strictly_positive(rate)

  with_seed(
    .seed,
    stats::rgamma(n=n,shape = shape, scale = scale)
  )
}



#' @name r_geom
#'
#' @title Generate Geometric Distributed Values
#'
#' @description
#' Generates a set of Geometric distributed values.
#'
#' @param prob
#' vector of probability of success, must strictly greater than 0 and
#' (non-strictly) less than 1, i.e. 0 < prob <= 1
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_geom(0.1)
#'
#' r_geom(seq(0.1,1,0.1))
#'
#' r_geom(0.1,n=10)
#'
#'
#' @export
r_geom <- function(prob=0.5,...,n=default_n(prob),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(prob)
  check_must_be_between(prob,0,1)
  with_seed(
    .seed,
    stats::rgeom(n=n,prob=prob)
  )
}


#' @name r_hyper
#'
#' @title Generate Hypergeometric Distributed Values
#'
#' @description
#' Generates a set of Hypergeometric distributed values.
#'
#' @param total
#' size of the population (e.g. number of balls)
#'
#' @param positives
#' number of elements with the desirable feature (e.g number of black balls)
#'
#' @param num
#' number of draws to make
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_hyper(10,5,5)
#'
#' r_hyper(10:20,10,5)
#'
#' r_hyper(10,5,5,n=10)
#'
#'
#' @export
r_hyper <- function(total,positives,num,
                    ...,n=default_n(total,positives,num),.seed=NULL){
  check_n(n)
  check_must_be_integer(total)
  check_must_be_integer(positives)
  check_must_be_integer(num)
  check_must_be_positive(total)
  check_must_be_between(positives,0,total)
  check_must_be_between(num,0,total)
  with_seed(
    .seed,
    stats::rhyper(nn = n,
                  m = positives,
                  n = total - positives,
                  k = num)
  )
}


#' @name r_lnorm
#'
#' @title Generate Log Normal Distributed Values
#'
#' @description
#' Generates a set of Log Normal distributed values.
#'
#' @param mean_log
#' vector of means (on the log scale)
#'
#' @param sd_log
#' vector of standard deviations (on the log scale), strictly positive
#'
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_lnorm(10)
#'
#' r_lnorm(10,2)
#'
#' r_lnorm(1:10)
#'
#' r_lnorm(-2,n=10)
#'
#'
#' @export
r_lnorm <- function(mean_log=0,sd_log=1,...,
                    n=default_n(mean_log,sd_log),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(sd_log)
  with_seed(
    .seed,
    stats::rlnorm(n=n,meanlog=mean_log,sdlog=sd_log)
  )
}



#' @name r_nbinom
#'
#' @title Generate Negative Binomial Distributed Values
#'
#' @description
#' Generates a set of Negative Binomial distributed values. Only two of `r`,
#' `prob` and `mu` can be provided.
#'
#' @note
#' It is important to note that this is the number of _failures_,
#' and not the number of _successes_, as in `rnbinom()`, so
#' `rnbinom(prob = x,...)` is equivalent to `r_nbinom(prob=1-x,...)`
#'
#' @param r
#' number of failure trials until stopping, strictly positive
#'
#' @param prob
#' vector of probabilities of success on each trial, between 0 & 1
#'
#' @param mu
#' vector of means
#'
#' @inheritParams r_norm
#' @inheritParams r_geom
#'
#' @examples
#'
#' set_n(5)
#'
#' r_nbinom(10,0.5)
#'
#' r_nbinom(1:10,mu=2)
#'#'
#' r_nbinom(10,0.2,n=10)
#'
#'
#' @export
r_nbinom <- function(r,prob=NULL,...,mu=NULL,n=default_n(r,prob,mu),.seed=NULL){
  check_n(n)
  r_provided <- !missing(r)
  prob_provided <- !missing(prob)
  mu_provided <- !missing(mu)

  if(sum(r_provided,prob_provided,mu_provided)==3)
    error_glue("Only two of r, prob or mu can be provided to r_nbinom()")

  if(sum(r_provided,prob_provided,mu_provided)<2)
    error_glue("Two of r, prob or mu are required in r_nbinom()")

  if(!r_provided) r <- mu*(1-prob)/prob
  if(!prob_provided) prob <- mu/(r + mu)

  check_must_be_strictly_positive(r)
  check_must_be_between(prob,0,1)

  with_seed(
    .seed,
    stats::rnbinom(n = n, size = r, prob = 1-prob)
  )
}


#' @name r_pois
#'
#' @title Generate Poisson Distributed Values
#'
#' @description
#' Generates a set of Poisson distributed values.
#'
#' @param rate
#' vector of rates, strictly positive
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_pois(10)
#'
#' r_pois(1:10)
#'
#' r_pois(10,n=10)
#'
#'
#' @export
r_pois <- function(rate,...,n=default_n(rate),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(rate)
  with_seed(
    .seed,
    stats::rpois(n = n, lambda = rate)
  )
}


#' @name r_tdist
#'
#' @title Generate T Distributed Values
#'
#' @description
#' Generates a set of Student's T distributed values.
#'
#' @param df
#' vector of degrees of freedom
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_tdist(10)
#'
#' r_tdist(1:10)
#'
#' r_tdist(10,n=10)
#'
#'
#' @export
r_tdist <- function(df,...,n=default_n(df),.seed=NULL){
  check_n(n)
  check_must_be_strictly_positive(df)
  with_seed(
    .seed,
    stats::rt(n=n,df=df)
  )
}


#' @name r_unif
#'
#' @title Generate Uniformly Distributed Values
#'
#' @description
#' Generates a set of Uniformly distributed values.
#'
#' @param min,max
#' vectors of lower and upper limits of the distribution
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_unif()
#'
#' r_unif(1:5,6:10)
#'
#' r_unif(1:5,10)
#'
#' r_unif(n=10)
#'
#'
#' @export
r_unif <- function(min=0,max=1,...,n=default_n(min,max),.seed=NULL){
  check_n(n)
  if(any(max < min)) error_glue("min must be less than max in r_unif()")
  with_seed(
    .seed,
    stats::runif(n=n,min=min,max=max)
  )
}


#' @name r_weibull
#'
#' @title Generate Weibull Distributed Values
#'
#' @description
#' Generates a set of Weibull distributed values.
#'
#' @details
#' This function provides alternative definitions for the `scale`
#' parameter depending on the user's parametrisation of the Weibull
#' distribution, with \eqn{k} = \code{shape}.
#'
#' Using \eqn{\lambda} = \code{scale}:
#' \deqn{
#' F(x) = 1 - exp(-(x/\lambda)^k)
#' }
#'
#' Using \eqn{b} = \code{b_scale}:
#' \deqn{
#' F(x) = 1 - exp(-bx^k)
#' }
#'
#' Using \eqn{\beta} = \code{B_scale}:
#' \deqn{
#' F(x) = 1 - exp(-(\beta x)^k)
#' }
#'
#' @param shape
#' vector of shape parameters, strictly positive
#'
#' @param scale
#' vector of scale parameters, strictly positive
#'
#' @param b_scale,B_scale
#' alternative definition of scale parameter, cannot be provided with
#' `scale`, strictly positive.
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(5)
#'
#' r_weibull(10)
#'
#' r_weibull(1:10,2)
#'
#' r_weibull(10,2)
#'
#' r_weibull(10,2,n=10)
#'
#'
#' @export
r_weibull <- function(shape,scale=1,...,b_scale=NULL,B_scale=NULL,
                      n=default_n(shape,scale,b_scale,B_scale),.seed=NULL){
  check_n(n)

  scale_provided <- !missing(scale)
  b_scale_provided <- !missing(b_scale)
  B_scale_provided <- !missing(B_scale)

  if(sum(scale_provided,b_scale_provided,B_scale_provided)>1)
    error_glue("Only one of scale, b_scale or B_scale can be provided to r_weibull()")

  if(b_scale_provided) scale <- b_scale^shape
  if(B_scale_provided) scale <- 1/B_scale


  check_must_be_strictly_positive(shape)
  if(scale_provided) check_must_be_strictly_positive(scale)
  if(b_scale_provided) check_must_be_strictly_positive(b_scale)
  if(B_scale_provided) check_must_be_strictly_positive(B_scale)


  with_seed(
    .seed,
    stats::rweibull(n=n,shape=shape,scale=scale)
  )
}



#' @name r_sample
#'
#' @title Generate Random Sample
#'
#' @description
#' Generates a Sample from a set, with replacement
#'
#' @param sample
#' a set of values to choose from
#'
#' @param weights
#' a vector of weights, must be the same length as `sample`,
#' between 0 & 1
#'
#' @inheritParams r_norm
#'
#' @examples
#'
#' set_n(15)
#'
#' r_sample(c("blue","red","yellow"))
#'
#' r_sample(c("blue","red","yellow"),
#'          weights=c(1,5,1))
#'
#' r_sample(c("blue","red","yellow"),n=10)
#'
#'
#' @export
r_sample <- function(sample,weights=NULL,...,
                     n=default_n(),.seed=NULL){
  check_n(n)

  weights_provided <- !missing(weights)

  len_sample <- length(sample)
  if(!weights_provided) weights <- rep(1/len_sample,len_sample)
  weights <- weights/sum(weights)

  if(length(weights) != len_sample)
    error_glue("Inconsistent parameter lengths supplied to r_sample()")


  with_seed(
    .seed,
    sample(size = n, x = sample, replace=TRUE, prob = weights)
  )

}
