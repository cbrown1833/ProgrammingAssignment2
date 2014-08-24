## The purpose of these functions is to cache a potentially
## computation intensive result so that it can be retrieved
## when needed instead of having to be computed each time

## makeCacheMatrix clears an stored value in m, creates a an
##   object to hold a cached inverted matrix, and defines
##   functions used by cacheSolve

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() { solve(x) }
  setmean <- function(mean) { m <<- mean }
  getmean <- function() { m }
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## cacheSolve accesses the object and returns the mean if it
##   is cached, or it computes the mean, caches it and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}