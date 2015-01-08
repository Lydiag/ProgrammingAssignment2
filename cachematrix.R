##Programming assignment

## makeCacheMatrix has the following functions
##1. Get the value of the matrix to be inverted
##2. Set the value of the matrix to be inverted
##3. Get the inverse of the matrix
##4. Set the inverse of the matrix

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


## CacheSolve does the following functions
## 1. Get the inverse from makeCacheMatrix function
## 2. If the inverse matrix is null, the find the inverse using solve() function (in-built)
## 3. Set the value of inverse in the majeCacheMatrix function -- Caching the inverse
## 4. If the inverse matrix is not null, then print the cached inverse matrix

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

##To check
##mat<-rbind(c(4,3),c(3,2))
##cac<-makeCacheMatrix(mat)
##First run
##cacheSolve(cac)
##[,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
##Second Run
##cacheSolve(cac)
##Getting the Cached Inverse Matrix.......
##[,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
