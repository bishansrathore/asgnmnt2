## Caching Inverse of a Matrix
## Matrix inversion is a costly computation 
## caching the inverse of a matrix is beneficical rather than compute it again and again.
## Below given are functions that create object and stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  invrs <- NULL
  set <- function(y) 
  {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function cacheSolve computes the inverse of the "matrix" created by 
## makeCacheMatrix function above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs))
  {
    message("getting cached data")
    return(invrs)
  }
  matrx <- x$get()
  invrs <- solve(matrx, ...)
  x$setInverse(invrs)
  invrs
}

