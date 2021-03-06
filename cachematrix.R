## 4/20/2014
## This function compute the inverse of a matrix and then cache the result for later use.
## How to run
## myM <- makeCacheMatrix( matrix(c(4,2,7,6),nrow=2,ncol=2))
## myMI <- cacheSolve(myM)
## running myMI <- cacheSolve(myM) again will trigger the cache

## This function store the matrix and the inverse cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## anonymous function - closures "They enlose the environment of the parent function and can access all it variables"
  ## this is to save the matrix to the environment   
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## anonoymous function to return the matirx
  get <- function() x
  
  ## Save the inverse of the matrix in cache
  setsolve <- function(solve) m <<- solve
  
  ## getting the matrix from the envirnoment.
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This is where we solve the inverse. if the inverse exist in cache return it else solve and store in cache. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  ## if m is not empty we return what is in the cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix 
  data <- x$get()
  
  ## solving and get the matrix inverse 
  m <- solve(data, ...)
  
  ## set the cache
  x$setsolve(m)
  
  m
}