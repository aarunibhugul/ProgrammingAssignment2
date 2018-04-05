## Put comments here that give an overall description of what your
## functions do

##Answere##
## Computing the Inverse of a Matrix:
## Computing the inverse of matrix is usually a costly computation. Also, there are some benefit 
## of caching the inverse of a matrix instead of computing it repeatedly.
## Below two functions will create a special object matrix which will store a matrix and calculate its inverse.

## Write a short comment describing this function:
##Answere
##The first function below, creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## The 2nd function computes the inverse of the special object "matrix" created by above by function makeCacheMatrix above. 
## If the inverse has already been calculated, the function should returns the inverse of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getInverse()
  if (!is.null(invr)) {
    message("calling cached data")
    return(invr)
  }
  mat <- x$get()
  invr <- solve(mat, ...)
  x$setInverse(invr)
  invr
}

