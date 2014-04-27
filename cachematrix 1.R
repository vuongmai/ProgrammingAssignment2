## 4/20/2014
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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

## Write a short comment describing this function
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


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

