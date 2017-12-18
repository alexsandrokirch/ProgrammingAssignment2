## Put comments here that give an overall description of what your
## functions do

## Write a short  comment describing this function
## This function  create a special object that stores a numeric matrix and cache's its inverse.

makeCacheMatrix <- function( x = matrix() ) {
              m <- NULL

              set <- function(y) {
                     x <<- y
                     m <<- NULL
              }     
           
              get <- function() x

              setinverse <- function(inverse) m <<- inverse

              getinverse <- function() m

              list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the mean of the special "matrix" created with the above function. 
## It checks to see if the inverse is already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ... ) {
              ### Return a matrix that is the inverse of 'x'
              m <- x$getinverse()

              if( !is.null(m) ) {

                 message("getting cached data")

                 return(m)
              } 

              data <- x$get()

              m <- solve(data, ...)

              x$setinverse(m)

              m
}

