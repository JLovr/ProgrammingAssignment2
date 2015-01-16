## creates a cached list that contains a matrix and its inverse
## which are employed by cachSolve.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## finds the inverse of an invertible matrix, using cached
## values if available, else calculating and caching the new value
## in a cached list managed by makeCacheMatrix.
## Generates an error if matrix is not invertible.
cacheSolve <- function(x, ...) {

  ## Return a matrix m that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
