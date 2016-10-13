## Put comments here that give an overall description of what your
## functions do
# x1 <- 1:4; dim(x1) <- c(2,2)
# aa <- makeCacheMatrix(x1)
# cacheInverse(aa)

makeCacheMatrix <- function(x = matrix()){
  # x is a square invertible matrix (ie solve(x) exists)
  # return a list with the following four ftns
  # get: returns matrix
  # set: sets the matrix
  # setInv: sets the matrix inverse
  # getInv: gets the matrix inverse
  
  
  # set field: inv to null
  inv <- NULL
  
  set <- function(y){
    # set field: x to input matrix y
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(mean) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

# cacheInverse calculates the inverse of the matrix object stored in makeCacheMatrix
# If inverse not yet calculated, it stores the inverse in the field "inv" of makeCacheMatrix object
# If inverse already calculated, it obtains inverse from field "inv" of makeCacheMatrix object

cacheInverse <- function(x, ...){
  # getting field "inv" from makeCacheMatrix object
  inv <- x$getInv()
  
  # inv==NULL only if "inv" field has not been set yet
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) #exits function if this line is called
  }
  
  #following lines are reached if "inv" field is NULL
  data <- x$get()
  #now inverse is calculated...
  inv <- solve(data, ...)
  #... and now stored. 
  x$setInv(inv)
  return(inv)
}
