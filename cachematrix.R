## Here one of the functions creates a special "matrix" object that can cache its inverse, and the otherfunction  
## computes the inverse of the special "matrix" returned by makeCacheMatrix and if the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse = function(inverse_mat) inverse <<- inverse_mat 
  getinverse = function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## And finally it returns the inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  
  
  ## if the inverse has already been calculated
  if (!is.null(inverse)){
    
    message("getting cached data")
    return(inverse) 
    
   } 
    
    
  ## if the inverse is not known then it is calculated.
  
  data <- x$get()
  
  inverse <- solve(data, ...)
  
  x$setinverse(inverse)
  
  return(inverse) 
    
}    
   
  
  
   
  

