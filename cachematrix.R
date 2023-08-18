## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This repository contains an R script that implements a custom matrix class with caching and inverse calculation functionality.
## The makeCacheMatrix function defines a matrix object with methods to set and get matrix values, as well as to calculate and store the matrix's inverse.
## The primary goal of this code is to optimize matrix inverse calculations by caching results and reusing them when needed.
## This approach can significantly enhance the performance of matrix operations and calculations in R, especially in scenarios involving iterative computations or repeated matrix manipulations.

# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
## This repository features an R script that introduces an efficient caching mechanism for calculating matrix inverses using the cacheSolve function. 
## The primary objective of this code is to optimize matrix inverse computations by leveraging cached results whenever possible,
## leading to substantial performance improvements in matrix-related calculations and operations.

# Function to compute the inverse of the special "matrix" and cache it
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}