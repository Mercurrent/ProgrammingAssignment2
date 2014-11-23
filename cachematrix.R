## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.

makeMatrix <- function(x = numeric()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinv <- function(invers) cache <<- invers
  getinv <- function() cache
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Creates a cache for an inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
  matr = makeMatrix(x)
}


## Takes a matrix out of the cache and return inverse of it.
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
