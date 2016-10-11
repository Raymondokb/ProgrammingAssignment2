## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# x is a matrix
# eg:
# x <- 1:4, dim(x) <- c(2,2)
# y <- makeCacheMatrix(x)
# y$set(x)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- solve(y)
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# eg: (from above)
# cacheSolve(y) will print out inverse of matrix x
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  m
}
