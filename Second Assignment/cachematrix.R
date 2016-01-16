## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the inverted matrix
  inv_x <- NULL
  
  # Set the value of the matrix
  set <- function(y){
    x <<- y
    inv_x <<- NULL
  }
  
  # Get the contents of the matrix
  get <- function() x
  
  # Set the inverse matrix
  setinverse <- function(y) inv_x <<- y
  
  # Return the inverse matrix
  getinverse <- function() inv_x
  
  # List with the function names to be called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get the inv_x field of object x
  inv_x <- x$getinverse()
  
  # Check if it's inverse matrix is already computed
  if(!is.null(inv_x)){
    print("Already computed. Returning it's contents!")
    return(inv_x)
  }
  
  # Not computed. Compute the inverse matrix and return
  y <- x$get()
  inv_x <- solve(y)
  x$setinverse(inv_x)
  inv_x
}
