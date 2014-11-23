## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly.

cache<-NULL

## Creates a cached for a matrix.

makeCacheMatrix <- function(x = matrix()) {
  cache<-ginv(x)
}


## Takes a matrix out of the cache and return inverse of it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  invers = makeCacheMatrix(x)
  invers  
}
