## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Assumes that x is always invertable, does not check to ensure that it is
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #get/set the base matrix
  set <- function(y) {
    x <<- y
    #invalidate cache if matrix is changed
    inv <<- NULL
  }
  get <- function() x
  
  #get/set the Inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  
  #make the functions accessable
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    # cache is already set, use it
    message("getting cached data")
    return(inv)
  }
  # cache not set yet, set it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
