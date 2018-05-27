makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    # set function
    
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }

    # get function
    get <- function() x

    # set the inverse
    setinverse <- function(inverse) inv <<- inverse

    # Get the inverse
    getinverse <- function() inv

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)	
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()

    if(!is.null(inv)) {
    	# Simply return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }

    data <- x$get()

    # Find the inverse
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inv)

    # Return this new result
    inv
}
