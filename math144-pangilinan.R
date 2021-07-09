makeCacheMatrix <- function(x = matrix()){
  mn <- NULL
  set <- function(y){
    x <<- y
     mn <<- NULL
  }
  get <- function() {x}
  setMean <- function(Mean) {mn <<- inverse}
  getMean <- function() {mn}
  list(set = set, get= get, setMean = setMean, getMean = getMean)
}

cacheSolve <- function(x, ...){
  mn <- x$getInverse()
  if(!is.null(mn)){
    message("getting cached mean data")
    return(mn)
  }
  mat <- x$get()
  mn <- solve(mat, ...)
  x$setMean(mn)
  mn
}
