
## 8th April 2020
## R programming Week 3
## Assignment 2


## the following code creates a pair of functions to allow an object to
## cache its inverse

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.



makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This second function of the pair computes the inverse of the 
## above special "matrix" created by the 'makeCacheMatrix' 
## If the inverse has already been calculated (and the 
## matrix has not changed), then it should simply retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## expected / test output from testing the functions
##
## Rweek2Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
##
## Rweek2Matrix$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##
## Rweek2Matrix$getInverse()
## NULL
##
## cacheSolve(Rweek2Matrix)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## cacheSolve(Rweek2Matrix)
## getting cached data
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

