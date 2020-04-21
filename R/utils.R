
##'
##'Evaluates an expression, returning a numeric value
##'
##'@param l the object (scalar or vector) to evaluate
##'@return a float evaluation of the expression
##'
##'@export
##'@examples
##' as_evaled_expression(c("2+2", "9*9")) -> (4, 81)
as_evaled_expression <- function(l) {
    if (is.null(l)) {
        stop("Cannot evaluate a NULL expression")
    }

    .as_evaled_expression <- function(obj) {
        if (is.numeric(obj)) {
            return(obj)
        } else if (is.character(obj)) {
            return(as.numeric(safe_eval(parse(text=obj))))
        } else {
            stop("expected numeric or string expression")
        }
    }

    return(unlist(lapply(l, .as_evaled_expression)))
}

#' The general density distribution
#'
#' @param dist character string, specification of the distribution of interest using the abbreviations from \code{\link[stats]{distributions}}
#' @param x numeric vector of quantiles
#' @param ... arguments specific to the distribution of interest (e.g. the "binom" distribution requires arguments for size and prob)
#' @param arg_list list, alternative to directly using the names of the arguments for the distribution of choice, the user can provide a list with those arguments enclosed. This is useful for when multiple distributions need to be drawn from.
#' @param lookup_dist logical, whether to look up the correctly abbreviated name
#' @param lookup_verbose logical, whether to print the abbreviated name found by lookup_dist
#'
#' @return a numeric vector representing the density of the distribution of interest
#' @export
#'
#' @examples
#' ## use curve and ddist to visualize a distribution
#' curve(ddist("lnorm", x, meanlog=1.23, sdlog=0.79), from=0, to=20)
ddist <- function(dist, x, ..., arg_list, lookup_dist=F, lookup_verbose=F){
    if(lookup_dist){
        data("dist_lookup_table")
        dist <- dist_lookup_table$dist[dist_lookup_table$name==tolower(dist)]
        if(lookup_verbose)
            message(paste0("using the abbreviation '", dist, "'"))
    }
    if(!missing(arg_list)){
        if(!missing(x)){
            arg_list[["x"]] <- x
        }
        return(do.call(paste0("d", dist), args = lapply(arg_list, as_evaled_expression)))
    } else
        return(do.call(paste0("d", dist), args = list(x=x, ...)))
}

#' The general distribution function
#'
#'
#'
#' @param dist character string, specification of the distribution of interest using the abbreviations from \code{\link[stats]{distributions}}
#' @param q numeric vector of quantiles
#' @param ... arguments specific to the distribution of interest (e.g. the "binom" distribution requires arguments for size and prob)
#' @param arg_list list, alternative to directly using the names of the arguments for the distribution of choice, the user can provide a list with those arguments enclosed. This is useful for when multiple distributions need to be drawn from.
#' @param lookup_dist logical, whether to look up the correctly abbreviated name
#' @param lookup_verbose logical, whether to print the abbreviated name found by lookup_dist
#'
#' @return a numeric vector representing the cumulative distribution function for the distribution of interest
#' @export
#'
#' @examples
#' ## use curve and pdist to visualize the cumulative distribution for a function
#' curve(pdist("norm", x, 0, 1), from=-5, to=5)
pdist <- function(dist, q, ..., arg_list, lookup_dist=F, lookup_verbose=F){
    if(lookup_dist){
        data("dist_lookup_table")
        dist <- dist_lookup_table$dist[dist_lookup_table$name==tolower(dist)]
        if(lookup_verbose)
            message(paste0("using the abbreviation '", dist, "'"))
    }
    if(!missing(arg_list)){
        if(!missing(q)){
            arg_list[["q"]] <- q
        }
        return(do.call(paste0("p", dist), args = lapply(arg_list, as_evaled_expression)))
    } else
        return(do.call(paste0("p", dist), args = list(q=q, ...)))
}

#' The general quantile function
#'
#' @param dist character string, specification of the distribution of interest using the abbreviations from \code{\link[stats]{distributions}}
#' @param p numeric vector of probabilities
#' @param ... arguments specific to the distribution of interest (e.g. the "binom" distribution requires arguments for size and prob)
#' @param arg_list list, alternative to directly using the names of the arguments for the distribution of choice, the user can provide a list with those arguments enclosed. This is useful for when multiple distributions need to be drawn from.
#' @param lookup_dist logical, whether to look up the correctly abbreviated name
#' @param lookup_verbose logical, whether to print the abbreviated name found by lookup_dist
#'
#' @return a numeric vector representing the quantiles of the distribution of interest
#' @export
#'
#' @examples
#' ## use curve and qdist to visualize the quantiles of a distribution
#' curve(qdist("pois", x, 3), from=0, to=1)
qdist <- function(dist, p, ..., arg_list, lookup_dist=F, lookup_verbose=F){
    if(lookup_dist){
        data("dist_lookup_table")
        dist <- dist_lookup_table$dist[dist_lookup_table$name==tolower(dist)]
        if(lookup_verbose)
            message(paste0("using the abbreviation '", dist, "'"))
    }
    if(!missing(arg_list)){
        if(!missing(p)){
            arg_list[["p"]] <- p
        }
        return(do.call(paste0("q", dist),
                       args = lapply(arg_list, as_evaled_expression)))
    } else
        return(do.call(paste0("q", dist), args = list(p=p, ...)))
}

#' Random generation for a specified distribution
#'
#' @param dist character string, specification of the distribution of interest using the abbreviations from \code{\link[stats]{distributions}}
#' @param n number of random values to return
#' @param ... arguments specific to the distribution of interest (e.g. the "binom" distribution requires arguments for size and prob)
#' @param arg_list list, alternative to directly using the names of the arguments for the distribution of choice, the user can provide a list with those arguments enclosed. This is useful for when multiple distributions need to be drawn from.
#' @param lookup_dist logical, whether to look up the correctly abbreviated name
#' @param lookup_verbose logical, whether to print the abbreviated name found by lookup_dist
#'
#' @return a numeric vector representing a random draw from the distribution of interest
#' @export
#'
#' @examples
#' ## create a function that adds two distributions together
#' two_dist_sum <- function(n_sims, dist1, dist1_args, dist2, dist2_args){
#' x1 <- rdist(dist1, n=n_sims, arg_list=dist1_args)
#' x2 <- rdist(dist2, n=n_sims, arg_list=dist2_args)
#' return(x1+x2)
#' }
#' ## use distribution to find time from exposure to covid-19 to hospitalization
#' ## using log-normal incubation time from Lauer, Grantz, et al.
#' ## and time from onset to hospitalization from Bi et al.
#' x <- two_dist_sum(n=1000,
#'                   dist1="lnorm",
#'                   dist1_args=list(meanlog=1.621,
#'                                   sdlog=0.418),
#'                   dist2="lnorm",
#'                   dist2_args=list(meanlog=1.23,
#'                                   sdlog=0.79))
#' summary(x)
#' ## now change incubation time to gamma distribution from Lauer, Grantz, et al.
#' x_gamma <- two_dist_sum(n_sims=1000
#'                         dist1="gamma",
#'                         dist1_args=list(shape=5.807,
#'                                         scale=0.948),
#'                         dist2="lnorm",
#'                         dist2_args=list(meanlog=1.23,
#'                                         sdlog=0.79))
#' summary(x_gamma)
rdist <- function(dist, n, ..., arg_list, lookup_dist=F, lookup_verbose=F){
    if(lookup_dist){
        data("dist_lookup_table")
        dist <- dist_lookup_table$dist[dist_lookup_table$name==tolower(dist)]
        if(lookup_verbose)
            message(paste0("using the abbreviation '", dist, "'"))
    }
    if(!missing(arg_list)){
        if(!missing(n)){
            arg_list[["n"]] <- n
        }
        return(do.call(paste0("r", dist),
                       args = lapply(arg_list, as_evaled_expression)))
    } else
        return(do.call(paste0("r", dist), args = list(n=n, ...)))
}


#'
#' Lookup table for distribution abbreviations
#'
#' A data frame with the full names and abbreviations for the distributions
#' in the `stats``
#'
#' @format data.frame with two columns
#' \itemize{
#'   \item \code{dist} the abbreviated name of the distribution
#'   \item \code{name} the full name of the distribution
#' }
#' @docType data
#' @name dist_lookup_table
#' @usage data(dist_lookup_table)
NULL