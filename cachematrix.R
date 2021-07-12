## Mapua's Data Science Coursera Requirement for Module 2: R Programming 
## Week 3 Assignment; A Partial Requirement for Data Science Module 2; GitHub user: CKFidelino

## This function will create a special "matrix" that could possibly cache the inverse

makeCacheMatrix <- function(c = matrix()) { ## emphasize and simplify the "matrix" default mode argument
  inv <- NULL                             ## initialize inv as NULL; where matrix inverse value will be held 
  set <- function(y) {                    ## new assigned function set from the set function
    c <<- y                             ## parent environment's value of its matrix
    inv <<- NULL                        ## reset inv to NULL if new matrix is identified
  }
  get <- function() x                     ## get function brief definition - argument where the matrix value is returned
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns an inv value in the parent environment
  getinverse <- function() inv                     ## where called returns the value of inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## This is required in order to refer ## to functions using the $ operator.
}


## The inverse of the special "matrix" provided by makeCacheMatrix is computed using this function.
## If you've previously computed the inverse (and the matrix has not changed),
## The inverse will then be retrieved from the cache by cacheSolve.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'c'
  inv <- c$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- c$get()
  inv <- solve(data, ...)
  c$setinverse(inv)
  inv
}
