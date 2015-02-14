#############################
## R Programming Assignment 2
## By Yanick Miron
## yan@eraid.biz
#############################

## This function creates a special "matrix" object that can cache its inverse.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # where the inversion will be store
  set <- function(y) { # set matrix value
    x <<- y
    m <<- NULL
  }
  get <- function() x # get matrix value
  set.inv <- function(inv) m <<- inv # set inverse matrix value
  get.inv <- function() m # get inverse matrix value
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  m <- x$get.inv() # checks if the inverse has already been computed
  if(!is.null(m)) { # if true, return the cache value
    message("getting cached data")
    return(m)
  }
  data <- x$get() # if false, inverse and set the result to cache
  m <- solve(data, ...)
  x$set.inv(m)
  m
}
