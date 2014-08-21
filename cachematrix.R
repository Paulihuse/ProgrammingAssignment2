## A function to cache the inverse of a matrix, so as to avoid needing to

makeCacheMatrix <- function(x = matrix()) {
  ## set the actual value of the matrix
  inverse<- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  
  ## get the inverse of the matrix
  list(set = set, get = get,
       
       setinverse = setinverse,
       getinverse = getinverse)
}


## A method to solve and cache finding the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x, the input matrix
  
  ## get the inverse       
  inverse <- x$getinverse()
  
  ## check if the inverse has been calculated
  
  if(!is.null(inverse)) {
    
    message("Retrieving inverse.")
    return(inverse)
    
  }
  ## calculate the inverse, since it has not been found
  input <- x$get()
  
  message("Solving for the inverse.")
  
  inverse <- solve(input, ...)
  
  ## cache the inverse
  x$setinverse(inverse)
  
  return(inverse)
}
