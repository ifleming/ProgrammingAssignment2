## These functions create a special object that stores a matrix and caches its inverse

## This function creates a special vector, which is a list containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## set the value of the matrix, make available outside of set function
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## get the value of the matrix
      get <- function() x
      ## set the value of the inverse, make available outside of get function
      setinverse <- function(solve) m <<- solve
      ## get the value of the inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function calculates the inverse of the special vector created by the makeCacheMartix function
## It first checks to see if the inverse has already been calculated, and displays it from cache if so
## Otherwise, it calculates the inverse and sets the value of the inverse in cache

cacheSolve <- function(x, ...) {
      ## check to see if inverse is already stored in cache, if so, return it and exit function
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if not cached, get the matrix, calculate, set, and display the inverse
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}