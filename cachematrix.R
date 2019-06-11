## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will define a special object that will store matrix and its inverse value. 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
		# when a new matrix is set, it will clear the cache value.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        setinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             setinverse = setinverse)

}


## Write a short comment describing this function
# This function will lookup if there is cached value of inverse.  
# If yes, it will return the value from cached.  
# If not, it will compute, store into cache and return the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$setinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

