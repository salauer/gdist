Install using:

    devtools::install_github("salauer/gdist", build_vignettes=T)

The problem
-----------

When writing functions that take draws from a certain distribution, it
is often difficult to add the flexibility to change distributions. In
the past, my functions, have looked something like this:

    some_function <- function(dist, shape, scale, meanlog, sdlog){
        if(dist=="gamma"){
            x <- rgamma(1000, shape=shape, scale=scale)
        }
        if(dist=="lnorm"){
            x <- rlnorm(1000, meanlog=meanlog, sdlog=sdlog)
        }
        if(!(dist %in% c("gamma", "lnorm"))){
            stop("dist must equal either 'gamma' or 'lnorm'.")
        }
        return(x^2+3*x-1)
    }

This is both time-consuming and inefficient, as incorporating new
distributions means adding more arguments and more front matter to the
code. It only gets worse if there are multiple distributions that need
to be drawn from.

    two_dist_sum <- function(dist1, shape1, scale1, meanlog1, sdlog1,
                             dist2, shape2, scale2, meanlog2, sdlog2){
        if(dist1=="gamma"){
            x1 <- rgamma(1000, shape=shape1, scale=scale1)
        }
        if(dist1=="lnorm"){
            x1 <- rlnorm(1000, meanlog=meanlog1, sdlog=sdlog1)
        }
        if(!(dist1 %in% c("gamma", "lnorm"))){
            stop("dist1 must equal either 'gamma' or 'lnorm'.")
        }
        if(dist2=="gamma"){
            x2 <- rgamma(1000, shape=shape2, scale=scale2)
        }
        if(dist2=="lnorm"){
            x2 <- rlnorm(1000, meanlog=meanlog2, sdlog=sdlog2)
        }
        if(!(dist2 %in% c("gamma", "lnorm"))){
            stop("dist2 must equal either 'gamma' or 'lnorm'.")
        }
        return(x1+x2)
    }

The `gdist` solution
--------------------

Using the `gdist` package, the user can specify their own distributions
and supply the arguments for those distributions as needed.

    library(gdist)
    some_function <- function(dist, ...){
        x <- rdist(dist=dist, 1000, ...)
        return(x^2+3*x-1)
    }

    y1 <- some_function("gamma", shape=5.8, scale=0.9)
    summary(y1)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.633  23.057  37.946  46.889  59.404 319.584

    y2 <- some_function("lnorm", meanlog=1.6, sdlog=0.4)
    summary(y2)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   5.432  23.988  37.827  46.492  58.373 362.825

This allows for more flexibility with less code.

When multiple distributions are required, use the `arg_list` to specify
the arguments for each distribution.

    two_dist_sum <- function(dist1, dist1_args, dist2, dist2_args){
        x1 <- rdist(dist1, arg_list=dist1_args)
        x2 <- rdist(dist2, arg_list=dist2_args)
        return(x1+x2)
    }

    y1 <- two_dist_sum(dist1="lnorm",
                       dist1_args=list(n=1000,
                                       meanlog=1.621,
                                       sdlog=0.418),
                       dist2="lnorm",
                       dist2_args=list(n=1000,
                                       meanlog=1.23,
                                       sdlog=0.79))
    summary(y1)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.430   6.965   9.278  10.094  11.986  56.273

    y2 <- two_dist_sum(dist1="gamma",
                       dist1_args=list(n=1000,
                                       shape=5.807,
                                       scale=0.948),
                       dist2="lnorm",
                       dist2_args=list(n=1000,
                                       meanlog=1.23,
                                       sdlog=0.79))
    summary(y2)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.435   6.779   9.378  10.313  12.403  62.443

You may want your users to have the same number of draws from each
distribution. `gdist` allows users to specify an `n` value for the
`rdist()` function (as well as `x`, `q`, and `p` for the `ddist()`,
`pdist()` and `qdist()` functions, respectively).

    two_dist_sum_n <- function(n_sims, dist1, dist1_args, dist2, dist2_args){
        x1 <- rdist(dist1, n=n_sims, arg_list=dist1_args)
        x2 <- rdist(dist2, n=n_sims, arg_list=dist2_args)
        return(x1+x2)
    }

    y1 <- two_dist_sum_n(n_sims=1000,
                         dist1="lnorm",
                         dist1_args=list(meanlog=1.621,
                                         sdlog=0.418),
                         dist2="lnorm",
                         dist2_args=list(meanlog=1.23,
                                         sdlog=0.79))
    summary(y1)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.746   6.802   8.915   9.967  11.806  80.939

    y2 <- two_dist_sum_n(n_sims=1000,
                         dist1="gamma",
                         dist1_args=list(shape=5.807,
                                         scale=0.948),
                         dist2="lnorm",
                         dist2_args=list(meanlog=1.23,
                                         sdlog=0.79))
    summary(y2)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.026   7.123   9.268  10.217  11.963  34.865

If you don’t know the abbreviated name for the distribution, you can
supply the full name of the distribution and – if it is in the `stats`
package – the functions will look it up for you when `lookup_dist=T`. To
learn the abbreviated name of the distribution, set `lookup_verbose=T`.

    rdist(dist="Log-Normal", n=1, meanlog=1.23, sdlog=0.79,
          lookup_dist=T, lookup_verbose=T)

    ## using the abbreviation 'lnorm'

    ## [1] 4.605414

Use `data(dist_lookup_table)` to see a list of the distributions from
the `stats` package and their abbreviations.
