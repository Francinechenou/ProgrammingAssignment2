setwd("C:/Users/CHENOU/Desktop/code_R")

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix(rnorm(16),4,4)) { ## define the argument with default mode of "matrix"
  inv <- NULL                             ## initialize inv as NULL; will hold value of matrix inverse 
  set <- function(y) {                    ## define the set function to assign new 
    x <<- y                             ## value of matrix in parent environment
    inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## define the get fucntion - returns value of the matrix argument
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
  ## to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#testing my function
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
#    [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getinverse()
#NULL
cacheSolve(my_matrix)
#    [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$getinverse()
#   [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5