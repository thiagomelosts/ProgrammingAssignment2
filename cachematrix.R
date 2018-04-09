## Programming Assignment 2: Lexical Scoping
## The goal is to create two functions for matrix inversion using lexical
## scoping to prevent unnecessary computations

## This function creates an object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix stored in the special object
## created by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}