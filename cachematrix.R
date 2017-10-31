## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## RubiconDS 2017-10-31

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function creates a special "matrix" object that can cache its inverse.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
