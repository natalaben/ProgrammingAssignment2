## These functions provide ability to compute the inverse of a matrix and store the 
## result in a cache.

## makeCacheMatrix creates a list of functions to hold and access
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}

## cacheSolve computes the inverse of an instance of the special matrix returned from
## makeCacheMatrix. The computation is only done once, subsequent calls are returned
## from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
