## Week 3: Caching the Inverse of a Matrix


## Creates a special "matrix", which is really a list containing a function to:
  # set the value of the matrix
  # get the value of the matrix
  # set the value of the inverse
  # get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # function to set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # function to get the value of the matrix
  get <- function() x
  # function to set the value of the inverse
  setsolve <- function(solve) m <<- solve
  # function to get the value of the inverse
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returs the inverse of the matrix. It recovers it from the cache if it es already stored, if not, it calculates it and it stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ## If we have the inverse already stored we return the cached data
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  } 
  ## If we don't have the inverse already stored we calculate it and we store it
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
