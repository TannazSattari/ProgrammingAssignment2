## Below are two functions that are used to create a special object
## that stores a square matrices and cache's its inverse.

## This function, makeCacheMatrix creates a special "matrix" object
## This object is really a list containing a function to
##setMatrix : the value of the matrix
##getMatrix : get  the value of the matrix
##setInverse: set the value of the Inverse
##getInverse: get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  
  Inv<- NULL
  setMatrix <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) Inv <<- inverse
  getInverse <- function() Inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


##This function, cacheSolve computes the inverse of the special "matrix"
##returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- x$getMatrix()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}

#Test Code:
squareMatrix <- matrix(runif(4,1,10),2,2)
# generate the makeCacheMatrix object with this matrix
invCacheObj <- makeCacheMatrix(squareMatrix)
# from now on calculate or retrieve calculated inversion using the cacheSolve function

cacheSolve(invCacheObj)
cacheSolve(invCacheObj)
cacheSolve(invCacheObj )

