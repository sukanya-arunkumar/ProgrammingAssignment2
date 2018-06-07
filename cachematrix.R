## This function computes the inverse of the matrix and cache it

makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrixinverse <<- solve
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## get the inverse of the matrix from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("Message : Retriving from cache")
  } else {
    mat <- x$get()
    matrixinverse <- solve(mat, ...)
    x$setinverse(matrixinverse)
  }
  return(matrixinverse)
}