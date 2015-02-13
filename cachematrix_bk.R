## R Programming Assignment 2
## By Yanick Miron
## yan@eraid.biz

## This function creates a special "matrix" object that can cache its inverse.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(inv) m <<- inv
  get.inv <- function() m
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$get.inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set.inv(m)
  m
}
