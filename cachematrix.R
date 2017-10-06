## Function makeCacheMatrix creates a special matrix object that can cache its inverse.
## The matrix object is really a list containing the functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(m = matrix()) {
  
    inv <- NULL
    
    ## set the value of the matrix
    set <- function(matrix) {
      m <<- matrix
      inv <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() m
    
    ## set the value of the inverse
    setInverse <- function(inverse) {
      inv <<- inverse
    }
    
    ## get the value of the inverse
    getInverse <- function() inv
    
    ## return matrix object
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
  
}

## Function cacheSolve computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix and sets the inverse value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  
    ## return inverse from cache
    inv <- x$getInverse()
    if(!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
    
    ## Calculate inverse and set inverse
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
    
    ## Return the inverse
    inv
}