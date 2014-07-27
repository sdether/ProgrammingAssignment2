## This file contains two functions for creating a matrix container than can cache the inverse operation of said matrix
## and a function to perform this inversion on that container

## makeCacheMatrix optionally takes a matrix and constructs a container that
## allows the setting and getting of the the contained matrix as well as the
## inverse of that matrix

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


## cacheSolve requires a cacheable matrix container and will either return
## the already contained cached version of the inverse of the contained
## matrix or compute the inverse, cache it in the container and then return it

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
