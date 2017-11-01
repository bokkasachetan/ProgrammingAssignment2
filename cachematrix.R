## Put comments here that give an overall description of what your
## functions do

## Fuctions to retrieve the cached inverse of matrix to avoid long calculative steps

makeCacheMatrix <- function(x = matrix()) {
## Creating preset values to the inv variable  
  inv <- NULL
## setting the lastest matrix, if any, to the x variable and resetting the inv to null
  setmat <- function(y=matrix()) {
    x <<- y
    inv <<- NULL
  }
## function to retrieve the matrix  
  getmat <- function() x
## function to set the inv of the matrix in the inv variable  
  setinv <- function(inv_cal) inv <<- inv_cal
## dunction to retrieve the inv  
  getinv <- function() inv
##return arguements  
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## check if not null, null is when a new matrix is entered, and return the inv from cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##if null, get the new data
  data <- x$getmat()
  ## calculate the inverse of the new matrix enetered
  inv <- solve(data, ...)
  #set the calculated inverse in the object x
  x$setinv(inv)
  ## return the calculated inverse
  inv
}

test_mat<-makeCacheMatrix(matrix(c(1,2,13,14),nrow = 2, ncol = 2))

cachesolve(test_mat)