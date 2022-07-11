## Creating two functions, one that creates an object that stores a numeric
## matrix and cache's its inverse. 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL ## Using i for "inverse" throughout the functions.
  set <- function(y) {
    x <<- y ##assigning value
    i <<- NULL ##assigning value
  } ## sets the value of the matrix
  get <- function() x ## gets the value of the matrix
  setinverse<-function(inverse) i<<-inverse ##sets the value of the inverse
  getinverse<-function() i ##gets the value of the inverse
  list(set=set, get=get,setinverse=setinverse, getinverse=getinverse)
}


## Checks the cache of inverse matrices to determine if the values have already
##been calculated. If yes, the function retrieves the inverse from the cache. 
## If no, it calculates the inverse and sets the inverse in the cache. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data") ## checking to see if the inverse
    ## has already been calculated before performing the solve function
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...) ##solve means to find the inverse
  x$setinverse(i) ##sets the values of the inverse in the cache
  i
}
