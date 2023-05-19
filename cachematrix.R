## This code contains functions that cache the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Calculate or retrieve inverse of the matrix from cache
  i <- NULL
  
  ## Update the value of the matrix 'x' in the cache and
  ## reset the inverse value to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Returns current value of matrix 'x' from the cache
  get <- function() x
  
  ## Set the inverse of matrix 'x' in the cache
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Return inverse of matrix 'x' from the cache
  getinverse <- function() {
    i
  }
  
  # Return value of the functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("From cache")  ## Retrieve from cache
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  message("Computed inverse")  ## Compute and cache the inverse
  i
}

## Testing
B <- matrix(c(1, 2, 3, 4), 2, 2)
A <- makeCacheMatrix(B)
cacheSolve(A)  ## Computes the inverse and caches it
cacheSolve(A)  ## Retrieves the inverse from the cache
