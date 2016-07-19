#to make matrix and able to cache inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inverst <- NULL 
  set <- function(y) {
    x <<- y
    inverst <<- NULL
  }
  get <- function() x
  setInv <- function(inverstMat) inverst <<- inverstMat # set inverse into cache
  getInv <- function() inverst # get inverse from cache
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# to calculate the inverst of matrix if it is not yet cache or just display the inverse if it is cached
cacheSolve <- function(mat, ...){
  inverst <- mat$getInv()
  if(!is.null(inverst)) { # check inverse if cached
    message("getting cached data")
    return(inverst)
  }
  ori <- mat$get()
  inverstCal <- solve(ori, ...) # calculate inverse if not cached
  mat$setInv(inverstCal)
  inverstCal
}
