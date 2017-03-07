## two functions which act as
## matrices and cache the invertible matrix
## inherited from 'Scoping Rule' of R

##make 'matrix' assign to variable x
##initialize inverse to NULL

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##get the matrix
  get <- function() x
  
  ##set inverse of matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ##return inverse property
  getInverse <- function() inv
  
  ##return list of methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

##Return a matrix that is the inverse of 'x'
  
  ##return matrix with inverse of x
  inv <- x$getInverse()

##if it already exists, return it
  if (!is.null(inv)) {
    message("Getting cached data . . . ")
    return(inv)
  }

##get matrix from object
##calculate inverse
##set inverse
##return matrix

  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}