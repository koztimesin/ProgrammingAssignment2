## Set the solved value "inv" as a null
## Then I changed every reference "mean" to "inv".
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
      ## Initialize the inverse property
      inv <- NULL  
      ## Input x as a matrix  
      
     
      set <- function(y){  ## Method to set the matrix

            x <<- y
            inv <<- NULL
      }
     
      get <- function() {x}   ## Method the get the matrix
      setInverse <- function(inverse) {inv <<- inverse}  ## Method to set the inverse of the matrix
      getInverse <- function() {inv}  ## Method to get the inverse of the matrix
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  ## Return a list of the methods
}

## Same there
## This function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...){
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      
      mat <- x$get()  ## Get the matrix from our object
      inv <- solve(mat, ...)  ## Calculate the inverse using matrix multiplication
      x$setInverse(inv)   ## Set the inverse to the object
      inv
}
