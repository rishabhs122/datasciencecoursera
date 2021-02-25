
# writing first function
makeCacheMatrix <- function(x = matrix()) {
  # make a var for null object
  invs <- NULL
  # setting the value of the matrix
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  # getting the value of the matrix
  get <- function() {x}
  
  # setting the value of the inverse
  setInverse <- function(inverse) {
    invs <<- inverse
  }
  # getting the value of the inverse
  getInverse <- function() {invs}
  # creating a list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# writing second function
cacheSolve <- function(x, ...) {
  # returns a matrix that is inverse or x
  invs <- x$getInverse()
  # checking that inverse is already calculated or not
  if (!is.null(invs)) {
    message("already calculated")
    return(invs)
  }# computing inverse of matrix and setting it
  mat <- x$get()
  invs <- solve(mat, ...)
  x$setInverse(invs)
  invs
}
