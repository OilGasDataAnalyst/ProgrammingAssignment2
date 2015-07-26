## caching the inverse of a invertible matrix
## functions do

## this function creates the inverse of the funcition in the cache memory section,
## the function will save the inverse in the cache area, calling m itself wont 
## bring up anything as its saved as the $getinverse of the cache....

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


## this will bring up the inverse from memory, if not it will simply solve for it their
## example code to run the following
## mat <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
## matrixx <- makeCacheMatrix(mat)
## cacheSolve(matrixx)
## matrixx$getinverse()

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
