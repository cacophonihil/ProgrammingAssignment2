## These functions enable the user to cache a matrix and 
## store it and it's inversion in a variable

## makeCacheMatrix is a function that will return a list
## of functions.  This list will contain 4 functions:
##		1. Set the matrix
##		2. Get the matrix 
##		3. Set the matrix inversion
##		4. Get the matrix inversion

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setInversion <- function(inversion) i <<- inversion
     getInversion <- function() i
     list(set = set, get = get, setInversion = setInversion, 
          getInversion = getInversion)
}


## cacheSolve will calculate the inverse of the matrix
## stored with makeCacheMatrix. It will first check to 
## see if an inverse matrix has already been calculated,
## if not, it will get the matrix, calculate the inverse,
## and then use setInversion to cache it in the variable
## that contains makeCacheMatrix.

cacheSolve <- function(x, ...) {
     i <- x$getInversion()
     if (!is.null(i)) {
          message("getting cached inversion")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setInversion(i)
     message("Inverting the matrix...")
     i
}
