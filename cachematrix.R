## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The function creates an object that caches the inverse matrix
##of the original one since computation is usually tedious

 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #Set the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Get the matrix
  get <- function() x
  #Set the inverse and get it
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  #Set results in a list
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
#After obtaining the matrix, the following function computes the
#inverse of it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  #get the matrix
  data <- x$get()
  #find the inverse
  inv <- solve(data)
  x$setinverse(inv)
  #New result to return
  inv
}

