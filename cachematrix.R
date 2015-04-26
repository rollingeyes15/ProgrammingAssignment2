# makeCacheMatrix is a function that returns a list of functions that stores a matrix
# and cached value of its inverse. These are the functions in the list:
#  setMatrix      set the value of a matrix
#  getMatrix      get the value of a matrix
#  cacheInverse   get the cahced value (inverse of the matrix)
#  getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  cache <-NULL
  
  setMatrix <- function(y){
    x<<-y
    cache <<-NULL
  }
  
  getMatrix <- function() x
  cacheInverse <- function(solvedInverse) cache<<-solvedInverse
  getInverse <- function() cache
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)

}


# cacheSolve function uses the special matrix from the makeCacheMatrix function from above
# Uses the cached inverse, if it exists already, else creates inverse and saves to cache
# Returns the inverse matrix of given matrix

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)){
          message("Getting cached Inverse Matrix")
          return(inverseMatrix)
        }
        
        tempMatrix <- x$getMatrix()
        inverseMatrix <- solve(tempMatrix)
        x$cacheInverse(inverseMatrix)
        inverseMatrix
}
