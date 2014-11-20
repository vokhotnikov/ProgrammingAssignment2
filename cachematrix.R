## Functions to allow for caching of result of a (potentially long-running)
## calculation of a matrix inverse. A solution for the programming assignment
## "2" of R Programming Coursera course.

## Creates a special "matrix" object that can cache its inverse.
## The object is internally a list of functions allowing to manipulate the 
## associated matrix data and its cached value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    
    list(set = set, 
         get = get, 
         setinv = setinv, 
         getinv = getinv)    
}

## Solves the special "matrix" object as created by makeCacheMatrix function
## Returns an inverse of associated matrix data, using the cached version of
## the result if available.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    mdata <- x$get()
    i <- solve(mdata, ...)
    x$setinv(i)
    i
}
