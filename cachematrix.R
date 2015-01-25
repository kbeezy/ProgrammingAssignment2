## As overall my function makes easier to find inverse of a square invertible matrix either it was 
## already solved before by the user or not. If the inverse matrix was already found for a 
## certain matrix, then the cachesolve will retrieve the inverse matrix from the cache. 


## The function creates a "matrix" that can cache its inverse and could be used later.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function calculates the inverse of the "matrix" that was already created. If the 
## inverse was already calculated, then the function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
