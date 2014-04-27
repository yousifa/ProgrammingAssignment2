## These functions create an invertible matrix and find  and return
## the inverse of a the matrix using caching.

## This function creates an invertible matrix object

makeCacheMatrix <- function(x = matrix()) {
     ## Inverse cache variable
     inver <- NULL
     ## Set function  to set matrix values
     set <- function(y) {
          x <<- y
          inver <<- NULL
     }
     ## Get function returns matrix values
     get <- function() x
     ## Function to store inverse in cache variable
     setinverse <- function(inverse) inver <<- inverse
     ## Returns inverse cache variable
     getinverse <- function() inver
     
     list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function takes in an invertible matrix and checks if the
## inverse is cached. If it is cached, it returns the cached
## inverse. If it is not cached, it finds the inverse then stores
## the value in cache. Then it returns the value.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## Check if inverse is in cache
     inver <- x$getinverse()
     if(!is.null(inver)) {
          message("getting cached inverse")
          return(inver)
     }
     ## Gets matrix data
     data <- x$get()
     ## Finds inverse of matrix
     inver <- solve(data, ...)
     ## Stores inverse in cache
     x$setinverse(inver)
     ## Returns inverse
     inver
}
