# Assignment 2 for the Coursera "R programming" course 
# Caching the Inverse of a Matrix
#
# As indicated, these functions are used to create a matrix object and calculate its inverse - 
# this value is stored and can be retrieved as long as the matrix does not change.

# first function, which creates a matrix object and stores its inverse

makeCacheMatrix <- function(x = numeric()) {
      # we create a square matrix filled with random numbers of dimensions NxN,
      # where N = x
      x <- matrix(runif(x^2),x,x)     
      m <- NULL                        # we clean the cache
                               # declare the functions that will be the output
      set <- function(y) {     # store out matrix  
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,       # output is a list of functions
           setinv = setinv,
           getinv = getinv)
}

# this functions looks in the memory to see if we already have the inverse of
# the matrix. If it's not there, it calculates the inverse.

cacheSolve <- function(x, ...) {
      m <- x$getinv()                     # retrieve data from memory
      if(!is.null(m)) {                   # if there's data, return that value
            message("getting cached data")
            return(m)
      }
      data <- x$get()                     # if there's no data, the solve and 
      m <- solve(data, ...)               # set that into the memory of the variable.
      x$setinv(m)
      m
}
