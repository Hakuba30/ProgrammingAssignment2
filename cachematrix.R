#makeCacheMatix: the function will create a special matrix object that can cache its inverse
#cacheSolve: the function will compute the inverse of the special matrix

#The following function creates a special "matrix", but actually is a list with
#functions to set and get the values of the given matrix and of its inverse
makeCacheMatix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

#This function will return the value of the special's "matrix" inverse by
#by retrieving from the cache
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
}

