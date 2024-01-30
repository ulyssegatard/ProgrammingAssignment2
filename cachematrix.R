## this module allows to avoid unnecessary computations by caching the inverse 
## of matrices

## creates an object to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    
}


## method to compute the inverse : if it was already cached, returns 
## cached version, otherwise, computes the inverses and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }
