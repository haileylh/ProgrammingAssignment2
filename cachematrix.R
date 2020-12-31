## I used the sample for makeVector and shifted the titles
## like "m" to "inver" and changed the argument to be a matrix rather 
## than numeric. Creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
  ## defines the argument as a matrix, x is initialized as the argument of the 
  ## function
  inver <- NULL
  ## sets the default for "inver" to be NULL for use later in the function
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  ## creates a nested function to set the value of the matrix, <<- assigns 
  ## the value on the right side of the operator to an object in the 
  ## parent environment
  get <- function() x
  ## sets the get function in order to get the value of the matrix
  setinverse <- function(inverse) inver <<- inverse
  ## sets the value of the inverse
  getinverse <- function() inver
  ## get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## elements are named so that you can access the functions by name with $
}

## Function computes the inverse from makeCacheMatrix, checks to see if the 
## inverse has already been calculated

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  ## sets "inver" for use later in the function
  if(!is.null(m)) {
    message("getting cached data")
    return(inver)
  }
  ## checks to see if the result is null
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
  ## Return a matrix that is the inverse of 'x'
}
