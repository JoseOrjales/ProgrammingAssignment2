## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object that caches 
# the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
        inverted = NULL
        # Set the matrix
        set = function(y) { 
                # `<<-` Assigns a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inverted <<- NULL
        }
        # Get the matrix
        get = function() x
        # Set inverse
        setinvert = function(inverse) inverted <<- inverse 
        # Get inverse
        getinvert = function() inverted
        # Then provide these functions as a list, 
        # sent to cacheSolve()
        list(set=set, get=get, setinvert=setinvert, getinvert=getinvert)
}


## cacheSolve works with makeCacheMatrix to compute the inverse
# of the special matrix returned by makeCacheMatrix. Where the 
# inverse has already been calculated (assuming no change to the 
# matrix, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted = x$getinvert()
        
        # When the inverse has already been calculated, just get
        # from the cache.
        if (!is.null(inverted)){
                # Skip the computation if inverse available.
                message("getting cached data")
                return(inverted)
        }
        
        # If cache not available, calculates the inverse.
        matrix.data = x$get()
        inverted = solve(matrix.data, ...)
        
        # Now set value of inverse in the cache with setinvert function.
        x$setinvert(inverted)
        
        return(inverted)
}