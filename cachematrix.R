## Pair of functions to calculate or retrieve the inverse of the object "matrix" depending on if the inverse of
## the "matrix" object has already been calculated or not

## Creates an object called "matrix" that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Returns the inverse of the "matrix" object.  If the inverse has already been calculated (and the matrix has not
## changed), then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  theMatrix <- x$get()
  inv <- solve(theMatrix, ...)
  x$setInverse(inv)
  inv
}