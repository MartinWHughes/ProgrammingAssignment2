## The following functions are used to caculate the inverse of a matrix. They
## cache the result to save time. To use, first create a cached Matrix using
## the makeCacheMatrix() function, then use the cacheSolve function to
## invert the Matrix

## Creates a cached matrix that stores both the matrix and the calculated inverse
## if previously calculated. The cached inverse is cleared if the data changes
## Arguments:
##    x: A square invertible matrix
## Returns: The cached matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of a matrix. To save time, it will use a cached inverse
## if the inverse has been previously solved and the data hasn't changed.
## Arguments:
##    x: A cached matrix created by makeCacheMatrix
## Returns: The inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) return(i)
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
