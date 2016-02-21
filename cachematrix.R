# The following two functions cache the inverse of a matrix by 
# 1) Creating a special matrix object that can cache its inverse
# 2) Check if matrix inverse has already been computed and if so return it with no further work.
#    Otherwise, compute the inverse, put it in cache, and then return the result.

# This function creates a special "matrix" object that can cache its inverse.
# Input: matrix
# Output: list (AKA, the special "matrix" object)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # Set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Get the matrix
    get <- function() x
    # Set the inverse matrix
    setinv <- function(inverse) inv <<- inverse
    # Get the inverse matrix
    getinv<- function() inv
    # Create and return a list that will be used as input to cacheSolve
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Attempt to get inverse of matrix 'x'
    inv <- x$getinv()
    # Check if inverse of matrix 'x' was found (i.e., has already be computed)
    if(!is.null(inv)) {
        # we have a hit (inverse is in cache), so return it with no further work
        message("getting cached data")
        return(inv)
    }
    # If we get here, then we have a miss (inverse of matrix 'x' has not yet been computed)
    data <- x$get()
    # Compute inverse and then add it to cache
    inv <- solve(data, ...)
    x$setinv(inv)
    # Return the now cached inverse of matrix 'x'
    inv
}
