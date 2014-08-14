## Author: Steeve Brechmann
## Course: R Programming
## Date: 18 July 2014

## The 'makeCacheMatrix' function create a matrix object 'x'
## with four (4) public methods: 'set', 'get', 'setSolve' and 'getSolve'
## Example: > mat <- makeCacheMatrix(matrix(rnorm(100), c(10, 10)))
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## 'set' create a new matrix in the 'x' object: 
  ## Example: > mat$set(matrix(rnorm(121), c(11, 11)))
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## 'get' retrieve the matrix in the 'x' object: 
  ## Example: > mat$get() return a 11-by-11 matrix 
  get <- function() x
  
  ## 'setSolve' find the matrix inverse: 
  ## Example: > mat$setSolve()
  setSolve <- function() m <<- solve(x)
  
  ## 'getSolve' retrieve the matrix inverse: 
  ## > mat$getSolve() return the inverse of the 11-by-11 matrix
  getSolve <- function() m
  
  ## Store all the public methods in a list for better access:
  list(set=set, get=get, setSolve=setSolve, getSolve=getSolve)
}

## The cacheSolve function retrieve the inverse matrix if it exists, 
## if not, it computes the inverse and store it for further access: 
cacheSolve <- function(x, ...) {
  ## Verify if the inverse matrix of 'x' is already computed
  m <- x$getSolve()
  if(!is.null(m)){
    message("Getting cached inverse")
    return(m)
  }
  
  ## Retrieve the 'x' object:
  data <- x$get()
  
  ## Compute the inverse of 'x':
  m <- solve(data)
  
  ## Set the inverse in the 'x' object:
  x$setSolve()
  
  ## Return the inverse
  m
}
