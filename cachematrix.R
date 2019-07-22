#Caching the Inverse of a Matrix: 
#The two functions compute the inverse of a Matrix
#by caching the result for retrieval and only
#calculating from scratch if the inverse has not yet 
#been calculated.

#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.
makeCachematrix <- function(x = matrix()) 
{
  inv_x <- NULL
  set <- function(y) 
  {
    x <<- y
    inv_x <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv_x <<- inverse
  getinv <- function() inv_x
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

#cacheSolve: 
#This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve should
#retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  inv_x <- x$getinv()
  if(!is.null(inv_x)) #inverse is retrieved from the cache:
  { 
    return(inv_x)
  }
  
  #otherwise inverse is calculated:
  X <- x$get()
  inv_x <- solve(X, ...)
  x$setinv(inv_x)
  
  inv_x
}


#Example:
#> source("cachematrix.R")
#> x <- matrix(1:4, nrow = 2, ncol = 2)
#> b <- makeCachematrix(x)
#> cacheSolve(b)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 