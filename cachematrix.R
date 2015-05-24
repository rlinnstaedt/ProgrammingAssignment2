## Write two functions that will accept an input matrix, calculate its
## inverse, store it, and return the solution or a message it's stored

## Create a function makeCacheMatrix that takes an input matrix and
## prepares it to be solved by function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a function cacheSolve that either returns the inverse of a matrix
## created by function makeCacheMatrix or a previously stored solution.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
