makeCacheMatrix <- function(x = matrix()){
      inv <- NULL  
      ## Input x as a matrix  
      ## Set the solved value "inv" as a null
      ## Then I changed every reference "mean" to "inv".

      set <- function(y){

            x <<- y
            inv <<- NULL
      }

      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Same there

cacheSolve <- function(x, ...){

      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }

      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}
