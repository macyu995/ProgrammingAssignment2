
## This program caches the inverse of a matrix.
## It avoids repeated calculations using lexical scoping.

## Create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setsolve <- function(solve) {
    m <<- solve
  }
  
  getsolve <- function() {
    m
  }
  
  list(
    set = set,
    get = get,
    setsolve = setsolve,
    getsolve = getsolve
  )
}


## Calculate the inverse of the special matrix.
## Return the cached inverse if it already exists.

cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setsolve(m)
  
  m
}