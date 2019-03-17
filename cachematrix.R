## the below two functions create an object that stores a matrix and cache's its inverse

## makeCacheMatrix creates a list containing functions to get and set the values of the matrix
## and to get and set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_inverse <<- inverse
  getinverse <- function() x_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse is laready computed, if so, it retrieves the cahced inverse
## else it computes the inverse and sets it in the cache

cacheSolve <- function(x, ...) {
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    message("getting cached inverse")
    return(x_inverse)
  }
  current_x <- x$get()
  x_inverse <- solve(current_x, ...)
  x$setinverse(x_inverse)
  x_inverse
}
