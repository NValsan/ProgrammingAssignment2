## makeCacheMatrix can be used to create a matrix and return its inverse.  
## cacheSolve will return the cached value of the invrse matrix if it doesn't already exist.
## If the inverse cache does not exist cacheSolve will calculate and return it.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## x is a matrix whose inverse can be calculated
  ## makeCacheMatrix can set the matrix value, get the value, set the inverse or get the inverse.
  ## set the inverted matrix object to NULL
  invmat<- NULL
  
  ## Set funtion to create matrix
  set<-function(y)
  {
    x<<- y
    invmat<<- NULL
  }
  
  ##Get value of matrix
  get<- function() x
  
  ## Set the inverse of the matix using solve()
  setinverse<- function(solve) invmat<<-solve
  
  ##Return the inverse of the matrix
  getinverse<-function()invmat
  
  ## List of functions that makecacheMatrix has
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Calculates the inverse of the matrix" returned by makeCacheMatrix() if the cache does not already exist
## Otherwise returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Assign value of input matrix to inmat object
  invmat<-x$getinverse()
  
        ## Check to see if inverse value exists. If it does, return inverse matrix
  if(!is.null(invmat))
  {
    message("returning cached matrix")
    return(invmat)
  }
      ## If there is no cached value, retrieve the matrix from the makeCacheMatrix, 
      ## calculate the inverse using solve and set value of invmat to the inverse matrix to cache it
  else
  {
    origmat<- x$get()
    invmat<-solve(origmat,...)
    x$setinverse(invmat)
    invmat
  }
}
