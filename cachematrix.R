## Use the two functions, makeCacheMatrix and cacheSolve, to create 
## input for and solve for an inverse matrix, then store the 
## solution in the cache for the next time cacheSolve function is 
## called.

## makeCacheMatrix creates a list of functions to be used to create 
## an inverse matrix based on a matrix argument.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) m <<- inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve function uses a function list created by makeCacheMatrix
## to solve for the inverse matrix of a given matrix.  It first checks
## to see if a solution is stored in the cache, and if so, returns that
## value.  If no solution is found, it will solve for the inverse matrix
## and return the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
