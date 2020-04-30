## Functions that cache the inverse of a matrix

## Makes special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  #Starts the inverse property
  inv <- NULL
  #Method to get and set the matrix
  mat_get <- function() x
  mat_set <- function(y){
    x <<- y
    inv <<- NULL
  }
  #Method to get and set the inverse of the matrix
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
  
  #Return a lost of the methods
  list(set = mat_set, get = mat_get, setInverse = setinv, getInverse = getinv)
}


## Should retrieve the inverse of the matrix computed from the makeCacheMatrix function above

cacheSolve <- function(x, ...) {
  #Returns a matrix that is the inverse of x
  inv_mat <- x$getInverse()
  #Returns the inverse only if it is already set
  if (!is.null(inv_mat)){
    message("getting cached data")
    return(inv_mat)
    #Get the data from the object
    matrix_data <- x$get()
    #Calculate the inverse 
    calc_inv <- solve(matrix_data, ...)
    #Set the inverse to the object
    x$setInverse(calc_inv)
    #Return the matrix
    return(calc_inv)
  }
}
