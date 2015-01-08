## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix<-NULL   #initialize inverse matrix to NULL
  setMatrix <- function(y) 
  {
    x<<-y
    invMatrix<<-NULL
    
  }
  getMatrix<-function()
  {
    x
  }
  setInverse<-function(inverseMatrix)
  {
    invMatrix<<-inverseMatrix
  }
  getInverse<-function()
  {
    invMatrix
  }
  list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverseMatrix<-x$getInverse()
        if(!is.null(inverseMatrix))
        {
           message("Getting the Cached Inverse Matrix.......")
           return(inverseMatrix)
        }
        inputMatrix<-x$getMatrix()
        inverseMatrix<-solve(inputMatrix)
        x$setInverse(inverseMatrix)
        inverseMatrix
        
}
