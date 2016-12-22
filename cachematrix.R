## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Return a list containing functions to:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         and use this list as the input for cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## return the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  cp.data <- x$get()
  inv <- solve(cp.data, ...)
  x$setinv(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
