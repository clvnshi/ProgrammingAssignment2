## Put comments here that give an overall description of what your
## functions do
## like the example provide, we would like to write a pair of 
## functions that cache the inverse of a matrix.


## Write a short comment describing this function
## makeCacheMatrix : creates a special "matrix"
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inver) inverseMatrix <<- inver
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cachesolve : calculate the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getInverse()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    data <- x$get()
    inver <- solve(data)
    x$setInverse(inver)
    inver
}
