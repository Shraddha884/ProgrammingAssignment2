## Matrix inversion is usually a costly computation and there are some
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly

## This function creates a special "matrix" object that can cache 
## its inverse

## MakechacheMatrix function will inverse the matrix and 
## store it in catche

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
      
    }
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix
## However, if the matrix is not stored in the catche, 
## setinverse function will inverse the matrix
## Inverse of a square matrix can be done with the solve function in R

cacheSolve <- function(x, ...) {
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting inverse cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
        ## Return a matrix that is the inverse of 'x'
    }
    
