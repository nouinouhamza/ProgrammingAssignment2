## These 2 functions create, calculate and cache the inverse of a matrix. 


## This function creates a special "matrix" object using 4 other functions


makeCacheMatrix <- function(x = matrix()) {
  ## Initial Value
  m <- NULL
  ## Set the Matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  ## Get the Matrix
  get <- function() x
  
  ## setinverse and getinverse are functions that simply store 
  ## the value (here is matrix) of the input in a variable m 
  setinverse <- function(solve)  m <<- solve 
  getinverse <- function()  m  
  
  ## Storing the 4 functions in makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function computes the inverse of
## the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'm'
  m <- x$getinverse()
  ## Return the inverse if its already set   
  if( !is.null(m) ) {    
    message("getting cached data")
    return(m)
  }
  ## in case the inverse isn't already set : getting the matrix 
  data <- x$get()
  ## Calculating the inverse using solve
  m <- m <- solve(data,...)
  x$setinverse(m)
  
  ## Return the matrix
  m
}
