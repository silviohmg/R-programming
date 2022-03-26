makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(turnMatrix) inv <<- turnMatrix
  getInv <- function() inv
  
  # stores the functions described above
  list(set = set, 
       get = get, 
       setInverse = setInv, 
       getInverse = getInv)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached mean...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

