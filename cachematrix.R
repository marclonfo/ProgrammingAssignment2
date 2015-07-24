## The two functions below set a matrix, compute its inverse and cache it, so that if the inverse of the same matrix 
## is requested it returns the cached inverse. 

## The function makeCacheMatrix set a matrix and it inverse, and also allow users to get the matrix or its inverse. 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
  
        setmatrix <- function (y){       ## This function set the value of the matrix
                       x <<- y
                       m <<- NULL
    
                  }
  
        getmatrix <- function () x      ## This function get the value of the matric
  
        setinverse <- function (inverse) m <<- inverse    ## This function set the inverse of the matric
  
        getinverse <- function() m          ## This function get the inverse of the matrix
  
        list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse= setinverse, getinverse=getinverse)
         ## makeCacheSolve returns a list with the four functions setmatrix, getmatrix, setinverse and getinverse
}


## The function cacheSolve() take an inversible matrix 'x'and first check if the inverse of that matrix is 
## in the cache. If yes it returns the message "Getting cached data" and the inverse in the cache otherwise   
## it calculate the inverse of the matrix 'x' using thefunction solve and return the inverse calculated.

cacheSolve <- function(x, ...) {
          
          m <- x$getinverse()
  
          if (!is.null(m)){
    
                  message("Getting cached data")
                  return (m)
           }
  
          data <- x$getmatrix()      ## Assign the matrix 'x' to the variable data
          m <- solve(data, ...)      ## calaculate the inverse of data and assign the result to m
          x$setinverse(m)            ## Set m as the inverse of the matrix 'x'
  
          m
  
}
