## Cached matrix builder and solver

## Convert that matrix to cached
makeCacheMatrix <- function(x = matrix()) {

  ## Initialize a variable to store inverse matrix
  inversedMat <- NULL
  
  ## Matrix setter
  set <- function(y){
    x <<- y
    inversedMat <<- NULL
  }
  
  ## Matrix getter
  get <- function(){
    x
  }
  
  ## Inverse matrix setter
  setInverse <- function(inverse) {
    inversedMat <<- inverse
  }
  
  ## Inverse matrix getter
  getInverse <- function() {
    inversedMat
  }
 
  ## Returning list of results.
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Solve a cached mat if it has not been calculated.
## It will retrieve from cache if has been calculated
## Else it will compute the inverse matrix.
cacheSolve <- function(x, ...) {
  ## Try to retrieve inverse
  inversedMat <- x$getInverse()
  
  ## If null value returned from the getter, 
  ## means it has not been calculated. Return the 
  ## cached value if it has been calculated.
  if(!is.null(inversedMat)) {
    message("getting cached data")
    return(inversedMat)
  }
  
  ## Proceed to calculation
  ## Retrieve matrix from getter
  data <- x$get()
  data
  ## Solve inverse
  inversedMat <- solve(data)
  
  ## Cache the result
  x$setInverse(inversedMat)
  
  ## Echo result
  inversedMat 
}

