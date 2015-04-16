## Usage: SPECIAL MATRIX CACHE OBJECT (list) <- makeCacheMatrix(matrix x, autoinverse=F)
##  Returns a special matrix cache object.
##  This is a list of functions that can be called on a matrix cache and an inverse matrix cache.
##  -Matrix x is a matrix for the inverse to be calculated on
##  -autoinverse is an optional logical value to specify whether to automatically create the inverse
##     By default, autoinverse is FALSE and the inverse will not be automatically calculated.
##

makeCacheMatrix <- function(x = matrix(), autoinverse=F) {
  #initialize a NULL value for the inverse cache
  m <- NULL
  # if autoinverse is TRUE, automatically calculates the inverse for the first time 
  if (autoinverse) { 
    m <-solve(x) 
  }
  
  # internal function to set the value of the matrix cache (assumes that y is differnt than the cache)
  set <- function(y) {
    x <<- y           #sets the value of cache x to the new matrix
    if (autoinverse)  #if autoinverse is specified then the inverse will
                      # automatically be created anytime the matrix values change
    {
      m <<- solve(y)
    } else {
    m <<- NULL  #resets the inverse cache since the matrix is (assumed to have) changed
    }
  }
  
  # internal function to retrieve the value of the matrix cache
  get <- function() x
  
  # internal function to set the value of the inverse matrix cache
  setinverse <- function(y)  m <<- y
  
  #returns the cached inverse matrix (or NULL) 
  getinverse <- function() m
    
  #clears the inverse cache //  useful for testing
  clearinverse <- function() m <<- NULL
    
  #lists the available functions that can be called
  list(set=set, get=get, setinverse = setinverse, getinverse = getinverse, clearinverse=clearinverse)

}


##
## Usage: matrix <- cacheSolve(x, ...)
##  x is a special matrix object created by makeCacheMatrix()
##  Returns the inverse of matrix x.
##  Uses as cached value of the inverse if x has not changed,
##  otherwise it calculates the inverse and caches the result.
##  
cacheSolve <- function(x, ...) {
  m <- x$getinverse()   #get the inverse value from cache (could be NULL)
  if(!is.null(m)) {  #test if the cache has already been set (inverse calculated)
    message("getting cached data")
    return(m)  #return from cache and exit function
  } 
  x$setinverse(solve(x$get())) #calculate and set inverse matrix in cache
  return(x$getinverse()) #get inverse from cache and return it
}
