## Write a short comment describing this function
## this func. creates a matrix that is able to cache matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function
## cachesolve computes the inverse of the matrix created by above.
## If the inverse has already been calculated then it should get back it from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}