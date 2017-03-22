## Here are two functions which can be used to save computation when
## computing the inverse of an invertible square matrix.
## 

## This function takes a matrix and creates a new "matrix" (really a list of four functions),
## which can store its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- NULL
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a "matrix" (of the type produced by makeCacheMatrix) and returns its inverse.
## Before the inverse is computed from the matrix, the function checks whether it has been done before.
## If it has, it returns the stored value. If it has not, it computes the inverse and stores it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
