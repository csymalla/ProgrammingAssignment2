## Set of functions to cache inverse of a matrix

## Creates matrix and list of functions

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                  x <<- y
                  inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Evaluates if inverse of matrix already processed
## uses cached value if processed or solves and
## caches new value if not processed.

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
