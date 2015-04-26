## These two functions together act as a solution for 
## addressing potentially time-consuming computation
## of calculating an inverse of a (large) matrix


## Function makeCacheMatrix initializes a list of functions. 
## This list acts like an object containing matrix in local 
## variable 'x' and cashed inverse of a matrix in local variable 'i'
## Functions contained in a list are getters and setters of these
## local variables.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a list created with a makeCacheMatrix
## function and returns an inverse of a matrix.
## It does so by first looking up for a cashed inverse and 
## only calculates inverse if the cashed value could not be found.

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    # returning cashed inverse
    return(i)
  }
  # if cashed value not found, calculate:
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
