## This module contains two functions that allow a user to calcualte and
## cache the inverse of a matrix

## Create a special matrix object that is able to cache its inverse.
## @param x A matrix that is cacheable

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(aInverse) inv <<- aInverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


## Computes the inverse of the special 'matrix' object. When the inverse has
## been previously calculated and the matrix has not change, then cacheSolve
## returns the cached inverse. Otherwise, the function calculates the matrix
## inverse and caches the result.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
