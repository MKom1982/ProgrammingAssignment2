## In the following code two functions are defined.
## For this assignment, it was assumed that the matrix supplied should
## always be invertible. Due to use of "solve" function, input matrix m
## must be square matrix.

## 1. function "makeCacheMatrix" creates a special "matrix" which caches
## the matrix being the inversion of the input matrix m

makeCacheMatrix <- function(m = matrix()){
    f1 <- NULL
    set <- function(n) {
        m <<- n
        f1 <<- NULL
    }
    get <- function() m
    setsolve <- function(solve) f1 <<- solve
    getsolve <- function() f1
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## 2. function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
    f1 <- m$getsolve()
    if(!is.null(f1)) {
        message("getting cached data")
        return(f1)
    }
    data <- m$get()
    f1 <- solve(data, ...)
    m$setsolve(f1)
    f1
}