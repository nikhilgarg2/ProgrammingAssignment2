
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_x <- NULL # this variable was declared to store the result of inverse of x
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL 
  }
  
  get <- function() x # this function the input matrix
  setInv <- function(inv) inverse_x <<- inv  # this function sets the inverse of the matrix
  getInv <- function() inverse_x # this function returns the inverse of the matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above.  If the inverse has already been calculated (and the matrix has
# not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv() # to get the inverse of the matrix from x
  if(!is.null(m)) { # to check if the result is not empty
    message("getting cached data") # we get catched data
    return(m) # return the precalculated inverse of the matrix
  }
  data <- x$get() # this step is reached in case we dont get the inverse of the matrix
  m <- solve(data) # this step calculates the inverse of the matrix solve == inverse calculation
  x$setInv(m) # Then we set the inverse of the matrix
  m # this returns the final result.
}

