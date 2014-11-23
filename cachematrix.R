## Functions to calculate and make storage for in inverse of a matrix


## For matrix x this function is creating a list of functions to: get the
## data from x, get the data from stored inverse matrix of x(if there, if not I=NULL),
## to set the data of x, and to set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
      x <<- y
      I <<- NULL
  } 
  get <- function() x
  setinverse <- function(Invers) I <<- Invers
  getinverse <- function() I
  list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}



## This function takes a list created with func above as an input, calculates
## the inverse matrix I and returnes it, in case I is not in cache already 
## and if the given matrix is square

cacheSolve <- function(x, ...) {
      I <- x$getinverse()
      if(!is.null(I)) {     ## checking to see if the stored value for I is NULL
        message("getting cached data")
        return(I)
      }
      data <- x$get()
      if(dim(data)[1]==dim(data)[2]) { ## if matrix is not square I will keep value NULL
        I <- solve(data, ...)
         x$setinverse(I)
      }  
      I
}
    
## Return a matrix that is the inverse of 'x'
