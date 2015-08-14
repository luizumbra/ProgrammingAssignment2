## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Input a matrix
# Output return a structured list of a matrix with values and the inverse mi'
makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  
  # Set completely values of x
  set <- function(y) {
    if (!is.matrix(y)) {
      message("The parameter is not a matrix.")
      return(NULL)
    }
    x <<- y
    mi <<- NULL
  }
  
  # Get values of x
  get <- function() {
    x
  }
  
  # Set the matrix inverse
  setInverse <- function(matrixInverse) {
    mi <<- matrixInverse
  }
  
  # Get the values of the inverse matrix
  getInverse <- function() {
    mi
  }
  
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

#Input: a matrix x
#Output: return the matrix inverso of x
cacheSolve <- function(x, ...) {
  ## Inverse Marix: A x A^(-1) = I. A^(-1) is the inverse matrix
  
  # Verify it was calculated the matrix inverse
  mi <- x$getInverse()
  if (!is.null(mi)) {
    message("Getting cached data.")
    return(mi)
  }
  
  # Use the function solve to calculate the inverse
  mi <- solve(x$get())
  
  # It call the function setInverse and return it
  x$setInverse(mi)
  mi
}
