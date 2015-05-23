## Put comments here that give an overall description of what your
## functions do
##
## Function to cache the inverse of a square matrix
##
## makeCacheMatrix(x=matrix()): This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve(makeCacheMatrix(x-matrix()): This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  # call function solve for square matrix inverse
  m <- solve(data, ...)
  x$setinv(m)
  m
}
