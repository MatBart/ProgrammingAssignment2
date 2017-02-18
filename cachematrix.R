# makeCacheMatrix is a function that returns a list of functions
# Store a matrix and a cached value of the inverse of the matrix
# Contains the following functions:
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   set the cached value (inverse of the matrix)
# * getInverse     get the cached value (inverse of the matrix)
#
makeCacheMatrix <- function(x = numeric()) {
  cache <- NULL
  
  # store matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # set cache 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# The following function calculates the inverse of a matrix created withmakeCacheMatrix
cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  # otherwise get the matrix, caclulate the inverse and store it in cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}