
## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y) {
        
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    set.inverse <- function(i) inverse <<- i
    
    get.inverse <- function() inverse
    
    list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the data and sets the value of the inverse in the cache via
## the set.inverse function.

cacheSolve <- function(x, ...) {
    
    inverse <- x$get.inverse()
    
    if (is.null(inverse)) {
        
        x$set.inverse(solve(x$get(), ...))
        inverse <- x$get.inverse()
    }
    
    inverse
}
