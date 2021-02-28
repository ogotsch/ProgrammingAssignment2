## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <- NULL
  }
  get <- function() x
  setinverse <- function(solve) invX <<- solve
  getinverse <- function() invX
  list (set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}

## Computes the inverse of the the special "matrix" returned by
## makeCacheMatrix; if inverse has already been calculated,
## retrieve inverse from cache

cacheSolve <- function(x, ...) {
   invX <- x$getinverse()
   if(!is.null(invX)) {
     message("getting cached data")
     return(invX)
   }
   data <- x$get()
   invX <- solve(data, ...)
   x$setinverse(invX)
   invX
}
