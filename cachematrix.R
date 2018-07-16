## Put comments here that give an overall description of what your
## functions do

## Creates list of getters and setters for a matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  setMatrix<-function(matrix){
    x<<-matrix
    inverse<<-null
  }
  getMatrix<-function() x
  setMatrixInv<-function(mInverse) inverse<<-mInverse
  getMatrixInv<-function() inverse
  list(set=setMatrix, get=getMatrix, setinverse=setMatrixInv, getinverse=getMatrixInv)
}


## Returns the inverse of a matrix from a cache, If cache doesnt exist it creates one.

cacheSolve <- function(x, ...) {
  inverse<- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data.")
    return(inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
