## These functions are used to calculate the inverse of a matrix and 
## cache the inverse. When the same matrix is calculated, the cacheSolve()
## function retrieves its inverse and return it without computing the 
## inverse again. When a new matrix is set by the makeCacheMatrix ()
## function, the cacheSolve() function calculates and cache its inverse.

## The following function returns a list of four functions that set and get
## the matrix and its inverse. When first called, a list object is returned,
## and the original matrix is stored in x.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function returns the inverse of the matrix which has been "processed"
## by the previous one. It does the calculation and cache jobs.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
