## Two functions are prowided in this work. The purpose of these functions is
## to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
## This "matrix" object is really a list containing a functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
    
    # function 1
    set <- function(y) 
      {
        x <<- y
        inv <<- NULL
      }
    
    # function 2
    get <-function() x
    
    # function 3
    setinverse <- function(inverse) inv <<- inverse
    
    # function 4
    getinverse <-function() inv
    
    # list of 4 functions is returned
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  }


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
        
  # First the function checks if inverse is not already saved in cache.
  # Also the value of inverse matrix is returned.
  inv <- x$getinverse()
  if(!is.null(inv)) 
    {
      message("getting cached data")
      return(inv)
    }
  
  # functions calculates and  sets inverse matrix in cache 
  # if it was not there already.
  # Also the value of inverse matrix is returned.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



## Test case

M <- makeCacheMatrix()
testMatrix <- matrix(c(1,2,3,4), 2,2) 
M$set(testMatrix)
cacheSolve(M)


