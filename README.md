
makeCacheMatrix <- function(x) {
  inver<- matrix(NA, dim(x)[1], dim(x)[2])
  i<-  matrix(NA, dim(x)[1],dim(x)[2])
  inver <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inver) i <<- inver
  getinver <- function() i
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  i <- x$getinver()
  if(!is.null(i)) 
  {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinver(i)
  i    ## Return a matrix that is the inverse of 'x'
}
