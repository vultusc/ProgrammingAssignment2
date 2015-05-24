## -----------------------------------------------------------------------------
## The goal is to realize two functions for calculate the inverse of a matrix
## and cache that value.
## -----------------------------------------------------------------------------
## makeCacheMatrix: take a matrix x in input and create a wrapper for x to hold
##                  its inverse matrix value.
##
## cacheSolve: take a special 'matrix' x in input and get back the value of the 
##             inverse matrix contained in it. If the inverse matrix is not
##             yet been calculated then the function cacheSolve calculate it
##             and update the value inside the special 'matrix' x.
## -----------------------------------------------------------------------------

## Create a wrapper object for the matrix x. This special 'matrix' object can 
## hold the matrix x and the calculated value of the inverse matrix of x.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  
  ## Set a new matrix and reset the previous inverse
  setMatrix <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  ## Return the current matrix
  getMatrix <- function() x
  
  ## Set the inverse of the current matrix, check inverseMatrix is a matrix
  setInverseMatrix <- function(inverseMatrix) {
    if (is.matrix(inverseMatrix)) {
      invMat <<- inverseMatrix
    }
  }
  
  ## Return the inverse matrix
  getInverseMatrix <- function() invMat
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## Given a special 'matrix' object, try to retrive the cached value of the 
## inverse matrix. If the inverse matrix is not found in cache then the function
## calculate it and insert the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverseMatrix()
  if (is.null(inverse)) {
    message("Inverse matrix not found, calculating...")
    matrix <- x$getMatrix()
    x$setInverseMatrix(solve(matrix, ...))
    inverse <- x$getInverseMatrix
  } else {
    message("Inverse matrix founded in cache...")
  }
  
  inverse
}
