makeCacheMatrix <- function(x = matrix()) {
        # makeCacheMatrix saves and returns a matrix, as well as its inverse.
        # Args:
        #   x: A matrix used to initialize internal copy.
        #
        # Returns:
        #     A list of functions that are defined in the function.
        
        inv <- NULL  # Initialize the inverse matrix.
        
        # Updates matrix
        set <- function(y) {  
                x <<- y       # Save y internal to makeCacheMatrix.
                inv <<- NULL  # Reset the inverse to null.  Matrix is updated.
        }
        
        # Return saved matrix
        get <- function() {
                x
        }
        
        # Save inverse matrix
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        
        # Return inverse matrix
        getinverse <- function() {
                inv
        }
        
        # Return list of functions defined above.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        # Args:
        #   x: List of arguments returned by makeCacheMetrics 
        inv <- x$getinverse()  # get inverse matrix
        if(!is.null(inv)) {    # If calculated inverse exists, return it.
                message("getting cached data")
                return(inv)
        }
        # inverse did not exist so calculate, save and return inverse.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}