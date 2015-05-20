
## Creates a matrix and then attaches functions to it

makeCacheMatrix <- function(x = matrix()) {
	# m is equal to nothing
	m <- NULL
        # set m to nothing and x to y
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # returns x
        get <- function() x
        # set m equal to inverse
        setinverse <- function(inverse) m <<- inverse
        # return m
        getinverse <- function() m
        # combine functions and return them
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# Returns a matrix that is the inverse of 'x', and
# if it isn't already cahced then calculate it

cacheSolve <- function(x, ...) {
       # set m equal to the inverse of x, pulled from the list
       # of functions attached to x
		   m <- x$getinverse()
       # if m exists then return m
       if(!is.null(m)) {
                message("getting cached data")
                return(m)
       }
       # set data equal to the matrix stored in x
       data <- x$get()
       # set me equal to the inverse of the input matrix
       m <- solve(data, ...)
       # retrieve the setinverse function, and call the
       # function, which sets the cache value of the inverse equal to m
       x$setinverse(m)
		   # return m
       m
}
