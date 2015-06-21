## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function 
## this function creates a list of four functions

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y = matrix()){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inversion) inverse <<- inversion
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, 
    getinverse = getinverse)
}


## Write a short comment describing this function
## this function will check if there is any cached inverse, if that is true
## it will return the inverse already cached, othersie
## it will calculate the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}