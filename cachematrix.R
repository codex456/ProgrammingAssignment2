## Create user defined retrivable object. 
## functions do

## makecacheMatrix creates an object that stores user defined matrix
## and inverses user defined matrix

makeCacheMatrix <- function(x = matrix()) {
  idx <- NULL
  set <- function(y) {
    x <<- y
    idx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) idx <<- inverse
  getinverse <- function() idx
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calls inverted matrix stored in makecacheMatrix
## message is diplayed if data is cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  idx <- x$getinverse()
  if (!is.null(idx)) {
    message("getting cached data")
    return(idx)
  }
  data <- x$get()
  idx <- solve(data, ...)
  x$setinverse(idx)
  idx
}
