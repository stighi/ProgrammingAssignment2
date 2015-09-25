## This file contains R code for the Coursera R-Progamming course, Assignment 2
## There are 2 functions, desciptions follow below


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

     inv <- NULL 
     set <- function(y) {
         x <<- y
         inv <<- NULL
     }
  
    ## return the matrix, x that was passed to the function
    get <- function() x 
  
    ## set the inverse of the matrix x
    setinv <- function () inv <<-solve(x)
    
    ## get the inverse of the matrix x  
    getinv <- function () inv
    
    ## all the functions stored inside the makeCacheMatrix function
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed)
## the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
      
      inv <- x$getinv()
      
      ## if cached then prints a message that informs the user that the inversion was cached
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      
      ## if not cached then create and return the inverse matrix
      datmatrix<- x$get()
      inv <- solve(datmatrix)
      x$setinv()
      message("inverse was not in cache")
      inv
  
  
}
