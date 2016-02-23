## Put comments here that give an overall description of what your
## functions do
## The following funtions are aimed to calculate the inverse of a matrix and in case that this operation was made,
## store the calculation to avoid recalculate the inverse (calculation in cache)

## Write a short comment describing this function
## A list containing function to get/set the matrix and get/set the inverse of the matrix is created

makeCacheMatrix <- function(x = matrix()) {
    invx <-NULL
    set<- function(y) {
        x <<- y
        invx <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverseMx) invx <<-inverseMx
    getinverse <- function() invx
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## The following function calculates the inverse of the matrix "x" if the inverse calculation was not performed. 
## If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getinverse()
    if(!is.null(invx)) {
        message("getting cached data")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data,...)
    x$setinverse(invx)
    invx
}
