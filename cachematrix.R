## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
## We assume that the matrix supplied is always invertible.

## It contains the following functions:

## setMatrix      sets the value of a matrix
## getMatrix      gets the value of a matrix
## cacheInverse   gets the cached value (inverse of the matrix)
## getInverse     gets the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {

  ## Holds the cached value or NULL if nothing is cached
  ## Initially nothing is cached, so set it to NULL
  
  cache <- NULL
  
  ## Store a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    
    ## since the matrix is assigned a new value, flush the cache
    
    cache <<- NULL
  }
  
  ## returns the stored matrix
  
  getMatrix <- function() {
    x
  }
  
  ## cache the given argument 
  
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  ## get the cached value
  
  getInverse <- function() {
    cache
  }
  
  ## return a list. Each named element of the list is a function
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get the cached value
  
  inverse <- x$getInverse()
  
  ## If a cached value exists return it
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Otherwise get the matrix, caclulate the inverse and store it in the cache
  
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  ## Return the inverse
  
  inverse
}
