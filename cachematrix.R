## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set & get function for matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## set & get function for matrix inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  ## return a list of all functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculate the inverse of square matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    ## If there is already the inverse of the matrix, get into this flow.
    message('getting cached data')
    return(inv)
  }
  
  ## If there is no existing inverse, the following code will be executed
  ## to calculate the inverse.
  data <- x$get()
  inv <- solve(data,...)
  
  ## Cache the inverse
  x$setinv(inv)
  ## Return the inverse
  inv
}
