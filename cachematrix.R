# Programming assignment 2 - Matrix inversion - to cache
# the inverse of a matrix rather than compute it repeatedly. 
# The below function, makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  #Initialize
  calcinv <- NULL
  # To set the value of matrix
  set <- function(y) {
    x <<- y
    calcinv <<- NULL
  }
  # To get the value of matrix
  get <- function() x
  # To set the value of inverse of the matrix
  setcalcinverse <- function(inverse) calcinv <<- inverse
  # To get the value of inverse of the matrix
  getcalcinverse <- function() calcinv
  # Create return list
  list(set=set, get=get, setcalcinverse=setcalcinverse, getcalcinverse=getcalcinverse)
}
# 
# The below function returns the inverse of the matrix. 
# The function first checks if the inverse has already been computed. 
# If it has been, it gets the result from the cache and skips calculation.
# If not, it computes the inverse and sets the value in the cache via
# the setcalcinverse function.
# As suggested in the assignment, this function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  # get inverse
  calcinv <- x$getcalcinverse()
  # Check if the inverse has already been computed
  if(!is.null(calcinv)) {
    print("Retreiving cached data....")
    return(calcinv)
  }
  # If cached inverse does not exist, get data
  rtrvdata <- x$get()
  # Calculate inverse
  calcinv <- solve(rtrvdata)
  # Set inverse
  x$setcalcinverse(calcinv)
  # Return calculated inverse
  calcinv
}