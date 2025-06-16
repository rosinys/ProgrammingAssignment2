
## creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get=get,
       setinv = setinv,
       getinv = getinv)
}


## calculates the inverse of the matrix, if it was already calculated before it will retrun the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i$setinv(i)
  i$setinv(i)
  i
}
