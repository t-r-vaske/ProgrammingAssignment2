## Functions for producing and taking the inverse of a matrix,
## while allowing for reuse of cached values in order to avoid 
## repeating potentially costly computations. These functions have 
## been modelled after functions 'makeVector' and 'cachemean'


## Function 'makeCacheMatrix()' for producing a "CacheMatrix," capable 
## of holding a cached value of its own inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list (set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## Function 'cacheSolve()' returns the inverse of a "CacheMatrix," 
## reusing an already computed and cached result when possible 
## (otherwise producing a "fresh" result with 'solve()')

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
