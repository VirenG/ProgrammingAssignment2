## This function creates a special matrix which is nothing but a list containing functions to get the matrix,
## set the matrix, setting the inverse and getting the inverse.


makeCacheMatrix <- function(x = matrix()) {
    inversex <- NULL
    set <- function(y) {
      x <<- y
      inversex <<- NULL
    }
    get <- function() x
    setinverse <- function(inversey) inversex <<- inversey
    getinverse <- function() inversex
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## This function computes inverse of the special matrix created from makeCacheMatrix function. However, it first 
## checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if (!is.null(inverse))
    { 
        message("getting cached data")
        return (inverse)
    }
    cachedmatrix <- x$get()
    inverse <- solve(cachedmatrix)
    x$setinverse(inverse)
    inverse
}
