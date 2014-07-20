## The functions together check if the inverse of the matrix under consideration has already been computed and cached. If it has already been computed and cached, then the cached value is returned and the inverse of the matrix is not recalculated. If the inverse of the matrix has never been computed, then it gets computed and cached for future use.
 

## The function below is used to store the inverted matrix and return it if the same matrix has to be inverted again

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,getmean = getmean)
}


## The funtion below is used to retrieve the inverted matrix if it has already been inverted and cached. If the matrix has never been inverted then the function computes the inverted matrix and calls the previous function (MakeCacheMatrix) to cache the value for future use.

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
