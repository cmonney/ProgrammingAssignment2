## The following R functions help us to cache the costly computation of matrix inversion.
## Aside of the makeCacheMatrix and cacheSolve, I have added a cacheTest and test functions to test my solution

## This function creates a special "matrix" object that can cache its inverse
## The function exposes set, get, setinversematrix and getinversematrix operations

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    
    get <- function() x
    
    setinversematrix <- function(inversematrix) m <<- inversematrix
    
    getinversematrix <- function() m
    
    list(set = set, get = get, setinversematrix = setinversematrix, getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    m <- x$getinversematrix()
  
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    m <- solve(data,...)
    
    x$setinversematrix(m)
    
    m
}

## Test function to check the two functions above
cacheTest <- function(mat) {
  
  tempMat <- makeCacheMatrix(mat)
  
  startTime <- Sys.time()
  cacheSolve(tempMat)
  duration <- Sys.time() - startTime
  print(duration)
  
  startTime <- Sys.time()
  cacheSolve(tempMat)
  duration <- Sys.time() - startTime
  print(duration)
  
}

test <- function() {
  set.seed(1678787)
  r = rnorm(1000000)
  mat = matrix(r, nrow=1000, ncol=1000)
  cacheTest(mat)
}
