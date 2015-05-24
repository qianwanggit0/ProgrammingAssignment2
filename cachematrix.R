
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse
# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # result of inversion stored here
  # setter function
  set <- function(y) {
    x <<- y
    xinv <<- NULL # init xinv to null
  }
  
  get <- function() x # return input matrix
  setInv <- function(inv) xinv <<- inv # set inversed matrix
  getInv <- function() xinv # return inversed matrix

  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { # if the result is there
    message("getting cached data")
    return(m) # return the calculated inversion
  }
  data <- x$get() # if not, we do x$get to get matrix 
  m <- solve(data) # solve it
  x$setInv(m) # set it to the object
  m # return result
}

# Computing the inverse of a square matrix can be done with the solve function
mat <- matrix(runif(25,0,500),5,5)
matCached <- makeCacheMatrix(mat)
matInv <- cacheSolve(matCached)
