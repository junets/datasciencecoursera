## THis function is designed to cache the inverse of a Matrix, 
## so that, the following function could read the one cached in memorym,
## which could save time for re-computing process.

## This function is to create the inverse of a matrix and cache it.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse
  getInverse <- function() inverse
  return(list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse))
}


## this function is to retrieve the inverse of the matrix, if it is not changed.
## But, if the inverse is changed, it would re-calculate the inverse of matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inverse)
  }
  matrix() <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  return(inverse)
}