##Assignment 2 - Wek 3

getwd()
source("makeVector.R")
source("cachemean.R")




## Description of what functions do...
## Together these functions cache the inverse of a matrix




## makeCacheMatrix description....
## makeCacheMatrix creates a special matrix object that can cache its inverse

## inverse_x - initialises the inverse property
## set - sets the matrix
## get - gets the matrix
## setinverse - sets the inverse of the matrix
## getinverse - gets the inverse of the matrix

## The <<- operator is used to assign a value to the parent environment 
## for example it assigns y to x in the parent environment

## Lexical scoping is used to retrieve the values in the nested functions

makeCacheMatrix <- function(x = matrix()) {
 
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set,
       get = get,
       setinverse = setinverse ,
       getinverse = getinverse)
  
}




## cacheSolve description...
## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## logic of how cacheSolve 
## inverse_x - returns matrix that's the inverse of 'x'
## if - returns inverse if it's already been set
## data - gets matrix from object

cacheSolve <- function(x, ...) {
    inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data) 
  x$setinverse(inverse_x)
  inverse_x
}




## Matrices for testing the R functions

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

m1 %*% n1

n1 %*% m1

solve(m1)

solve(n1)

myMatrix_object <- makeCacheMatrix(m1)

cacheSolve(myMatrix_object)



n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

myMatrix_object$set(n2)

cacheSolve(myMatrix_object)

cacheSolve(myMatrix_object)
