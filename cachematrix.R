#This R script takes a matrix as an input and 
#returns the inverse of the matrix. The inverse is cached the first time
#it is evaluated and from then on, it is returned from the cache
#Two functions are used for this purpose - makeCacheMatrix and cachSolve
#The makeCacheMatrix is a formal argument to the cachsolve function

#The makeCacheMatrix takes a matrix as an argument and returns a list
# containing the following 4 functions
#1.A function to set the matrix
#2.A function to get the matrix
#3.A function to set the inverse of the original matrix
#4.A function to get the inverse of the original matrix
# This function uses the operator '<<-' to set the value of the 
#inverse. The operator '<<-' looks back in the enclosing environment 
#for an environment that contains the variable m and assigns the right
#hand side to the its value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The cachSolve function takes the makeCacheMatrix as an argument.
#This function checks if the inverse exists in cache. If it exists it returns 
#the existing inverse else it uses solve() function to calculate the
#inverse and returns the inverse.

cacheSolve <- function(makeCacheMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'...
  m <- makeCacheMatrix$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- makeCacheMatrix$get()
  m <- solve(data, ...)
  makeCacheMatrix$setinverse(m)
  m
}
