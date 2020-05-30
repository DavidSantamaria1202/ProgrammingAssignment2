## The first function receives an inversible matrix, here 4 different 
## functions are created and then saved in a list.
## The second function takes as argument a variable which posses the list
## of functions and will calculate the inverse only if it wasn't done before.
## In case the inverse matrix was calculated earlier, it will search for
## the value in the cache, making use of the functions established in the 
## first function makeCacheMatrix.

## This function receives as parameter the matrix to be solved.
## It creates and returns a list of function that will
##    1) Allows to change the value of the parameter (matrix) determined
##       in the funciton.
##    2) Get the info from the parameter (matrix).
##    3) Save in the cache the inverse of the matrix.
##    4) Retrieve the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  setMat <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  getMat <- function() x
  
  getInv <- function() inverse
    
  setInv <- function(inv) inverse <<- inv

  list(setMat = setMat, getMat= getMat, setInv = setInv, getInv = getInv)
}


## This function receives as a parameter a saved variable with the result
## of the function makeCacheMatrix.
## First the function retrieves if the amtrix has already been solved,
## if the answer is yes, then it prints the saved inverse of the matrix
## Else the function retrieves the matrix from the function makeCacheMatrix
## then it solves for its inverse and finally saves this value in the
## argument variable by calling the setInv() function of makeCacheMatrix.
## The function returns the invese of the matrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  else{
    matx <- x$getMat()
    inverse <- solve(matx)
    x$setInv(inverse)
    inverse
  }
}
