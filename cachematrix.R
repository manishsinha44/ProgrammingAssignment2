## function to create a new matrix, and return 4 function to get the value of the matrix, 
## set the value of the matrix, get the cached inverse of the matrix, and store the inverse in cache

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL ##initiating cached value to NULL
  ##function to store the value of the matrix
  set <- function(y) 
    { 
      x <<- y  
      i <<- NULL  #flushing the cache to store value for new matrix
    }
  ## function to return the value of the matrix
  get <- function() x
  ## function to cache the result
  setinverse <- function(inverse) 
    i <<- inverse
  ##function to retrieve the cached value
  getinverse <- function() i
  ## the output of this function is a list of functions
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ##calling the getinverse() function
  i <- x$getinverse()
  ##fetching the cached value if there is a value in the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ##if no cached value is found, inverse is calculated
  data <- x$get() ## getting the value of the matrix
  i <- solve(data) ## calculating the inverse through solve() function
  x$setinverse(i) ## caching the value of the inverese
  i  ##returning the inverse as the result of this function
}