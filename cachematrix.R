
## This function makeCacheMatrix perform the following functions on the matrix X
## a: Set the matrix
## b: Get the matrix
## c: Set the inverse of the matrix
## d: Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the Inverse
  Inv <- NULL
  ## Set the matrix
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setInv <- function(solve) Inv <<- solve
  
  ## Get the inverse of the matrix
  getInv <- function() Inv
  
  ## Make a list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  Inv <- x$getInv()
## Check if cached version exists, if it exists return it instead
    if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
}
