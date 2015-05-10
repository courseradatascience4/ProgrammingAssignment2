
# This function accepts a matrix.
# It produces a matrix "object" that can cache its inverse.
# Four functions are returned in a list. These can be used to access or modify the inverse of the provided matrix.

makeCacheMatrix <- function(x = matrix()) {
  # i represents the inverse of x.
  # i of the provided matrix has not been calculated yet
  i <<- NULL
  
  # replace the stored matrix with a new one (y)
  # this resets i as i is yet to be calculated for this new matrix.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # used to cache the inverse for this matrix once calculated
  setinverse <- function(inverse) i <<- inverse
  
  # returns the "original" matrix
  get <- function() x
  
  # returns the inverted matrix
  getinverse <- function() i
  
  # list of functions available on matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



# This function accepts a special matrix object produced by makeCacheMatrix. 
# Returns the inverse of this matrix.
# The inverse is calculated only if the inverse has not already been calculated & cached for this matrix.

cacheSolve <- function(x, ...) {
  #checks if the inverse of x has already been calculated & cached
  i <- x$getinverse()
  
  # if i has been populated, the inverse of xis already calculated & nothing needs to be done
  # the inverse can be returned without any further work
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # otherwise we need to get the "original" matrix & solve it
  data <- x$get()
  i <- solve(data, ...)
  
  # finally, we cache the inverted matrix
  x$setinverse(i)
  
  i
}