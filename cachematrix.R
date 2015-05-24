## These functions create a cached list of functions for a given matrix and
## return the inverse of the matrix from that cache 

## makeCacheMatrix takes a matrix and creates a list to hold the functions
## needed to store and return the inverse of that matrix

makeCacheMatrix <- function(cm = matrix()) {

  inv <- NULL
  mset <- function(newmatrix){
    cm <<- newmatrix
    inv <<- NULL
  }
  mget <- function() cm
  setinv <- function(newinv) inv <<- newinv
  getinv <- function() inv
  list(mset=mset, mget=mget, setinv=setinv,getinv= getinv)
  
  
}


## cacheSolve 1. accepts a list created by makeCacheMatrix,
##            2a. returns the value of the inverse if it exists or
##            2b. calculates the inverse and stores it in the list, and
##            3.  returns the inverse


cacheSolve <- function(mCM, ...) {
        ## Return a matrix that is the inverse of 'x'
  cs <- mCM$getinv()
  if(!is.null(cs)) {
    message("getting cached data")
    return(cs)
  }
  newmat <- mCM$mget()
  cs <- solve(newmat, ...)
  mCM$setinv(cs)
  cs
}
