## Make CacheMatrix creates a list which contains the following functions:
# 1. set which sets the value of the matrix
# 2. get which returns the matrix
# 3. setinverse which saves the inverse matrix
# 4. getinverse which returns the inverse matrix

#
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


#The fuction cacheSolve returns the inverse of the matrix but first
#checks if the operation has been performed and if it has returns the value
#from the cache if not calculates the inverse saves its value in the cache.
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
