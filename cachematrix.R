## makeCacheMatrix is a function that will cache the inverse of
## a matrix and the second part of the script will validate if it is already stored. If it is
## then it will use the same inverse matrix already in the cache.
## If not, then it will calculate the inverse of the matrix.

## First, define a place to hold the cached results. It is NULL because it has not been run yet
## also the variable for the matrix is defined

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function calculates the inverse of a the input matrix created in the previous step

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
