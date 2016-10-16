## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# We are trying to optimize one of the most difficult problems. The inverse
# of a big matrix is a big data problem. We have to find ways to make things
# fast and exact. 

# makeCacheMatrix is a function that gives us a list of functions. This is 
# a way to use Scope method to solve in some way the inverse problem. 

makeCacheMatrix <- function(x = matrix()) {
  # set the cache value 
  inver <- NULL
  
  # store a matrix 
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  
  # returns the stored matrix 
  get <- function() {
    x
  }
  
  # cache the solve function
  setinverse <- function(solve) {
    inver <<- solve
  }
  
  # get the cached value
  getinverse <- function() {
    inver
  }
  
  # return a list of functions 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cachesolve is a function that calculates the inverse method 

cacheSolve <- function(x, ...) 
{
  # get the cached value
  inverse <- x$getinverse()
  
  # if there is a cached value, returns the matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # we calculate the inverse of the matrix if we had no cache value
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  
  # return the inverse
  inverse
}