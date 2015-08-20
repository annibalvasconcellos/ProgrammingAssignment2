## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute it repeatedly
## the following two functions are used to cache the inverse of a matrix

## The function makeCacheMatrix creates a special "vector",
## which is really a list containing a function to:
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y)
        {
          x <<- y
          inv <<- NULL
        }
  get <- function() {x}
  setinversa <- function(inversa) {inv <<- inversa}
  getinversa <- function() {inv}
  list(set = set, 
       get = get,
       setinversa = setinversa,
       getinversa = getinversa)
  
}


## The following function "cachesolve" returns the inverse of the matrix
## If the inverse has been calculated and is still valid, the function
## just gets the inverse from the cache.
## Otherwise, it computes the inverse, sets the value in the cache,
## and returns the inverse

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinversa()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinversa(inv)
  inv  
}
