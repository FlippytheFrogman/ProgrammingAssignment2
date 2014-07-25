makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special "matrix" object that can cache its inverse
        ## (## Program demonstrates R lexical scoping capabities.)
        ## It provides functions that save and return a matrix, and its inverse.
        ## These functions can be called external to the makeCacheMatrix function
        ## through a list of functions retured to the caller of this routine

        ## makeCacheMatrix
        ##
        ## Args:
        ##      x: A matrix used to initialize internal copy.
        ##         If arg not passed in, a zero by zero matrix is used
        ##
        ## Returns:
        ##      A list of functions that are defined in the function that can
        ##

        inv <- NULL  ## Initialize the inv (inverse) variable.

        set <- function(y) {
                ## Overwrites the saved matrix
                ## Arg:
                ##       y: Matrix to save.
                ##
                ## Returns:
                ##       NULL
                x <<- y         ## Save y external to set function and
                ## internal to makeCacheMatrix.
                inv <<- NULL    ## Reset the inv (inverse) to null.
        }

        get <- function() {
                ## Returns saved matrix
                x
        }

        setinverse <- function(inverse) {
                ## saves inverse matrix
                ## Arg:
                ##       inverse: Inverse matrix to be saved.
                ##
                ## Returns:
                ##       Inverse matrix passed to function
                inv <<- inverse  ## save in lexically scoped variable inv.
        }

        getinverse <- function() {
                ## Returns inverse matrix
                ## Returns:
                ##       Previously saved inverse matrix
                inv
        }

        ## Return list of functions defined above.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Computes the inverse of the special "matrix"
        ## returned by makeCacheMatrix above. If the inverse has already
        ## been calculated (and the matrix has not changed),
        ## then the cachesolve should retrieve the inverse from the cache.
        ## Return a matrix that is the inverse of 'x'
        ## Args:
        ##   x: List of arguments returned by makeCacheMetrics
        ##   remaining ... arguments will be passed to solve function.
        inv <- x$getinverse()  ## get inverse matrix
        if(!is.null(inv)) {    ## If inverse already exists, return it.
                message("getting cached data")
                return(inv)
        }
        ## inverse did not exist so calculate, save and return inverse.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
