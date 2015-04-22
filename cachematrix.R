## In this script there are two functions: 1) returns caches an inverse of a supplied matrix, and 2) computes the inverse of a square matrix.

## This function creates a Cache of the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
cacheStore <- NULL
set <- function(y) {
      x <<- y
      cacheStore <<- NULL   
}
get <- function() x
setinverse <- function(inverse) cacheStore <<- inverse
getinverse <- function() cacheStore
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function creates a Cache of the inverse of a matrix.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(cacheStore)) {
    message ("getting cached data")
    return(cacheStore)
  }
  data <- x$get()
  cacheStore <- solve(data, ...)
  x$setinverse(cacheStore)
  cacheStore
}
