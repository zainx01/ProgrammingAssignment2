## The function makeCacheMatrix defined below creates a special matrix which has the ability to cache it's inverse. 
## The second function computes the inverse of the special matix and if the inverse has already been calculated and the matrix unchanged, it retrieves the inverse from cache.

## The matrix contains a list of functions which allow setting and getting the matrix as well as setting and getting the matrix inverse from cache.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()  x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will check whether the matrix inverse already exists using the getinverse function. If it does, the cache stored inverse will be returned, otherwise a new matrix inverse will be computed, stored in the cache for further use and then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}