## This program gives the Inverse of the matrix as the output. Enter a invertible 
## matrix and call function makeCacheMatrix. If inverse is not created in the
## makeCacheMatrix it is done in the cacheSolve matrix
## Caching the inverse of the matrix helps to save valuable processing time. This
##is absolutely important if the data under consideration(here matrix) is huge and 
## the computer processor is not powerful enough to maintain the optimal speed.

##In both of the matrix functions the global variable 'i' is the Inverse of the
##matrix
#********************************************************************************#
##Function computes the inverse of the special "matrix" and can cache the inverse
##The function takes a mtrix as arguments and can return the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#********************************************************************************#

## This function caches the inverse matrix to save time.
## It checks if the inverse is calculated by the makeCacheMatrix.
##if not then it finds inverse and returns the inverse
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i     ## Returning a matrix that is the inverse of the matrix 'x'
}
