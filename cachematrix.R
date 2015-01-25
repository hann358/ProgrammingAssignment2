## makeCacheMatrix creates a list that is a function to:
##1. set the value of the vector
##2. get the values of the vector
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix


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

## cacheSolve calculates the inverse of a matrix, but first checks to see
##if the inverse has already been calculated. If so, it gets the inverse from the cache
##and skips the computation.
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
