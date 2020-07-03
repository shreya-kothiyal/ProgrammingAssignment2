##The following function is to create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x ##to call the function(x)
  setInverse <- function(inverse) j <<- inverse ##to set the value of the matrix
  getInverse <- function() j ##to call the matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  j <- x$getInverse() ##to get the inverse of the matrix
  ##if condition to check whether the inverse of matix already been calculated.
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j  ##to print the value of inverse matrix
}
