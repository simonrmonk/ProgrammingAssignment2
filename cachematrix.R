## The below two functions are used to create a special object that stores a matrix and cache's its inverse

## Create a matrix of functions that:
  ## 1) sets the value of the matrix
  ## 2) gets the value of the matrix
  ## 3) sets the value of the inverse of the matrix
  ## 4) gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
        x <<- y
        m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## If the inverse of 'x' has already been cached, returned the cached data
  ## otherwise calculate the inverse of x and return x

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

x = matrix(c(7,0,-3,2, 3, 4, 1, -1, -2),3,3)
test <- makeCacheMatrix(x)
cacheSolve(test)
