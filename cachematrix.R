## Alberto De Obeso Orendain July 2014
## Programming Assignment 2 - Coursera R Programming Course
## Given that it makes sense to cache time-consuming computations, the following functions calculate 
## the inverse of matrix and cache it in memory. 

# makeCacheMatrix creates a list containing functions to:
# (a) set the value of the matrix
# (b) get the value of the matrix
# (c) set the value of the inverse of the matrix
# (d) get the value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), then  
# cachesolve retrieves the inverse matrix from the cache.
# Assumptions: the matrix supplied is always invertible
# Restrictions: use the solve(X) function

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
