makeCacheMatrix <- function(x = matrix()) {
  inverst <- NULL
  set <- function(y) {
    x <<- y
    inverst <<- NULL
  }
  get <- function() x
  setInv <- function(inverstMat) inverst <<- inverstMat
  getInv <- function() inverst
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(mat, ...){
  inverst <- mat$getInv()
  if(!is.null(inverst)) {
    message("getting cached data")
    return(inverst)
  }
  ori <- mat$get()
  inverstCal <- solve(ori, ...)
  mat$setInv(inverstCal)
  inverstCal
}
