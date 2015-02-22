## This file contains a pair of functions that can be used 
## to create a special "matrix" object and cache its 
## inverse. The first function contains a set of functions 
## that can be used to create a "matrix" and calculate its 
## inverse, and the second function uses the functions defined
## in the first to calculate the inverse of the "matrix" (if
## it has not been calculated before) or retrieve it from cache
## if the operation has already been performed before.

## makeCacheMatrix creates a special "matrix" object and returns
## a list of four functions for it.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # Set the value of the "matrix" x to user specified 
        # matrix y and set the inverse to null since it is not 
        # known yet. (both assignments done in parent environment)
        x <<- y
        inv <<- NULL
    }
    # get function to return the contents of the "matrix"
    get <- function() x
    # setinv function sets the cached inverse "inv" 
    setinv <- function(inverse) inv <<- inverse
    # getinv function returns the cached inverse (can be null)
    getinv <- function() inv
    # return list of 4 above functions for "matrix" object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve calculates "matrix" inverse or retrieves
## it from cache.

cacheSolve <- function(x, ...) {
    # Retrieve variable containing inverse
    inv <- x$getinv()
    
    # If retrieved variable is not null, the inverse exists already.
    # Return this inverse from cache.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If the function has not returned yet, inverse is null.
    # Get the actual matrix using x's get function.
    data <- x$get()
    # Calculate inverse of matrix using solve.
    inv <- solve(data, ...)
    # Save computed inverse in "cache".
    x$setinv(inv)
    # Return inverse.
    inv
}
