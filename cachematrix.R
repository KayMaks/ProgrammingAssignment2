## A pair of function that can create an object which can cache its inverse and
## retrieve it

## The makeCacheMatrix function can create an object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  return(list(set = set, 
              get = get,
              setinverse = setinverse,
              getinverse = getinverse))
}


## The cacheSolve function is used to retrieve the object that was cached, if
## the inverse was calculated with the previous function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
        ## Return a matrix that is the inverse of 'x'
}
