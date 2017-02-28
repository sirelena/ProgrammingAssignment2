## Week 3: Caching the Inverse of a Matrix


## Creates a special "matrix", which is really a list containing a function to:
  # set the value of the matrix
  # get the value of the matrix
  # set the value of the inverse
  # get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get the value of the matrix
  get <- function() x
  # function to set the value of the inverse
  setinv <- function(inverse) inv <<- solve
  # function to get the value of the inverse
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returs the inverse of the matrix. It recovers it from the cache if it es already stored, if not, it calculates it and it stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## If we have the inverse already stored we return the cached data
  if(!is.null(inv)) { 
    message("getting cached data")
    return(inv)
  } 
  ## If we don't have the inverse already stored we calculate it and we store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
