# Matrix inversion is usually a costly computation and there may be some benefit to caching the 
# inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to write a pair of functions 
# that cache the inverse of a matrix.

# Write the following functions:
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.


# -----------------------------------------------------------------------------------------------
# Function - <makeCacheMatrix>
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# -----------------------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inver <- NULL
  
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  ## Method the get the matrix
  get <- function() x
  ## Return the matrix
  
  ## Method to set the inverse of the matrix
  setinverse <- function(inverse) inver <<- inverse
  
  ## Method to get the inverse
  getinverse <- function() inver 
  
  ## Return a list of the methods
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}



# -----------------------------------------------------------------------------------------------
# Function - <cacheSolve>
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
# -----------------------------------------------------------------------------------------------


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getinverse()
  
  ## Just return the inverse if its already set
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  ## Get the matrix from our object
  
  inver <- solve(data, ...)
  
  ## Set the inverse to the object
  x$setinverse(inver)
  
  ## Return the matrix
  inver        
}

# Sample Solution
# > x = rbind(c(5, 20), c(20, 5))
# > m = makeCacheMatrix(x)
# > m$get()
# > cacheSolve(m)
# > cacheSolve(m)
# > x = rbind(c(5, 20), c(20, 5))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2]
# [1,]    5   20
# [2,]   20    5
# > cacheSolve(m)
# [,1]        [,2]
# [1,] -0.01333333  0.05333333
# [2,]  0.05333333 -0.01333333
# > cacheSolve(m)
# getting cached data
# [,1]        [,2]
# [1,] -0.01333333  0.05333333
# [2,]  0.05333333 -0.01333333