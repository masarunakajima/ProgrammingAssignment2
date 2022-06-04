## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the matrix
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  
  # simply return the matrix
  get <-function() x

  # set inverse matrix
  setinv <- function(inver) inv<<- inver
  
  # return the inverse matrix
  getinv <- function() inv
  
  #return the cache enabled matrix
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # Get the cached inverse
    inv <- x$getinv()
    
    # if there is a cached inverse, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    # If no inverse is cached, compute the inverse
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
