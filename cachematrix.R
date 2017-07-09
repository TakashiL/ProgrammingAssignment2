## Below are two functions that are used to create a special "matrix" object that stores a matrix 
## and cache its inverse.

## The first function creates a special "matrix" object, which is really a list
## of 4 functions to
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the inverse of the matrix;
## 4. get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function calculates the inverse of matrix in the special "matrix" object.
## Also, it will check the cache first, if the inverse is already there, it won't do the calculation.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## as we assume that the matrix supplied is always invertible, we can always use solve(data) here
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
