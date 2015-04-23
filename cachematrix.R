## In this script there are two functions: 1) returns caches an inverse of a supplied matrix, and 2) computes the inverse of a square matrix.

## This function (makeCacheMatrix) creates a the matrix which will then be stored in cache for easier and quick access. 
makeCacheMatrix <- function(x = matrix()) {
cacheStore <- NULL
set <- function(y) {
      x <<- y
      cacheStore <<- NULL   
}
  ## gets and sets the inverse of the matrix.
  get <- function() x
  setinverse <- function(inverse) cacheStore <<- inverse
  getinverse <- function() cacheStore
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function (cacheSolve) leverages the makeCacheMatrix above and returns and solves for the
## inverse of a matrix. 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x' from cache.
  m <- x$getinverse()
  if(!is.null(cacheStore)) {
    message ("getting cached data")
    return(cacheStore)
  }
    ## computes the inverse of the matrix and returns the results from cache as needed.
    data <- x$get()
    cacheStore <- solve(data, ...)
    x$setinverse(cacheStore)
    cacheStore
}
