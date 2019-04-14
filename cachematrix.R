makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL ## It will give the inverse matrix, until the matrix is inverse this value is set NULL
    
    
    set <- function(y) { ## setting the values in the Cache, so can be called in cacheSolve 
        x <<- y
        inverse <<- NULL
    }
    get <- function() x 
    setInverse <- function(solve) { ## this is the function that solve the inverse matrix
      inverse <<- solve
    }
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## create a list that can be used in the cacheSolve
}


cacheSolve <- function(x, ...) {
      inverse <- x$getinverse ## getting the matrix   
      if(!is.null(inverse)){ ## testing if the results is already in the cache
        message("getting cache data")
        return(inverse)
      }
      d <- x$get() ## getting the matrix 
      inverse <- solve(d) 
      x$setInverse(inverse) ## solving the inverse matrix 
      inverse ## printing the result
}
