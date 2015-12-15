## These two functions cache the inverse of a square invertible matrix. To use, first run the 
## makeCacheMatrix with your matrix of interest and save to an object. Next, use the cacheSolve 
## with the object as the input, which should output the inverse of the matrix. 

## The first function takes a matrix as the input and creates a list of 4 functions: the set function 
## allows you to change the input matrix, the get function displays the input matrix, the setinv 
## function allows you to manually input the inverse matrix and the getinv function displays the
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function takes the object above and does one of two things. If there is already an inverse
## matrix, it returns it (and prints 'getting cached data'). Otherwise it gets the matrix, uses 
## the solve function to obtain the inverse matrix, saves that matrix and outputs it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
