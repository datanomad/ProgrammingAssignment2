## This file contains a pair of functions that cache the inverse of a matrix.
## (Matrix inversion is usually a costly computation that can be avoided by
## caching the inverse of a matrix rather than computing it repeatedly.)

## The function makeCacheMatrix creates a special matrix object that can
## cache its inverse.

## The function cacheSolve computes the inverse of the special matrix
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will retrieve the inverse
## from the cache, to avoid re-computing the inverse unnecessarily.


makeCacheMatrix <- function(x = matrix()) {
## Initialize the variables where the matrix and 
## the inverse of the matrix will be cached:
    mcache <- x
    imcache <- NULL
## A function to set the value of the matrix (and clear the cache):
    set.matrix <- function(y) {
        x <<- y
    }
## A function to get the value of the matrix:
    get.matrix <- function() { x }
## A function to cache the value of the inverse of the matrix:
    set.cache <- function(im) { 
        imcache <<- im
    }
## A function to get the value of the inverse of the matrix:
    get.cache <- function() { imcache }
## A function to reset the cache of the matrix:
    reset.mcache <- function(m) {
        mcache <<- m
    }
## Return a list of functions:
    list(set.matrix = set.matrix, get.matrix = get.matrix,
         set.cache = set.cache, get.cache = get.cache,
         reset.mcache = reset.mcache)
}


cacheSolve <- function(x, ...) {
## Get the matrix:
    m <- x$get.matrix()
## Get the cache:
    mcache <- get("mcache", envir = environment(x$set.matrix))
    imc <- x$get.cache()
## Check if the matrix has changed:
    samesame <- identical(m, mcache)
## If the matrix has not changed, then check if the inverse has been 
## calculated before (in which case, the cache is not empty):
    if(samesame==TRUE & !is.null(imc)) {
        message("getting cached data...")
        return(imc)
    }
## Otherwise, if the matrix has not changed but the cache is empty
## or if the matrix has changed...
    else if(samesame==TRUE & is.null(imc) | samesame==FALSE) {
        ## Reset the matrix in the cache:
        x$reset.mcache(m)
        ## Calculate the inverse of the matrix:
        im <- solve(m, ...)
        ## Place the inverse of the matrix in the cache:
        x$set.cache(im)
        ## Return the computed inverse of the matrix:
        im 
    }
}
