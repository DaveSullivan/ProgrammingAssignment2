## makeCacheMatrix(x) and cacheSolve(x, ...) work together to create a special
## list object that caches a matrix and its inverse


## makeCacheMatrix(x) creates a special matrix which is really a list containing 4 functions
##                    if x is supplied is is expected to be a matrix
##                    the 4 functions are
## set(y) :   sets the cached matrix to the parameter y and clears the cached inverse
##            y is expected to be a matrix
## get() :    returns the cached matrix
## setinv():  sets the cached inverse matrix passed in
##            inverse is expected to be the inverse of the cached matrix x
## getinv():  gets the cached inverse matrix. 
##

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## cacheSolve() returns the inverse of the special matrix that makeCacheMatrix creates
## if the inverse has already been computed it is returned
## if the inverse has not yet been computed, it is computed, cached, and returned
## The parameter x is expected to be the special list returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv = x$getinv()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    m = x$get()
    inv <- solve(m)
    x$setinv(inv)
    inv  
}
