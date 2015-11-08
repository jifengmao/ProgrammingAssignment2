# Matrix inversion is usually a costly computation and there is
# some benefit to caching the inverse of a matrix rather than 
# computing it repeatedly. The functions below contains pair of functions 
# that cache the inverse of a matrix.

# This function creates a special "matrix" object that can 
# cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  mInverse <- NULL
  MatrixData <- NULL
  set <- function(InMatrix) { # set Matrix and its inverse to initial state
    MatrixData <<- InMatrix
    mInverse <<- NULL
  }
  
  get <- function() MatrixData # return saved orignal matrix
  
  setInverse <- function(InversedData){
    mInverse <<- InversedData
  }
  
  getInverse <- function() return(mInverse)

  #Use the paramter to set the MatrixData
  set(x)
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then cacheSolve 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  cacheInverse <- x$getInverse()
  if(!is.null(cacheInverse)){
    message("Already inversed, return the result in cache.")
    return (cacheInverse)
  }
  else {
    cacheInverse <- solve(x$get())
    x$setInverse(cacheInverse)
    message("NOT inversed before, solve matrix on the fly")
    return (cacheInverse)
  }
}


#unit test 1
#TestMatrix <- matrix(1:4,2,2)
#SavedMatrix <-makeCacheMatrix(TestMatrix)
#cacheSolve(SavedMatrix) #should have message "NOT inversed before, solve matrix on the fly"
#cacheSolve(SavedMatrix) #should have message "Already inversed, return the result in cache."

#unit test 2
#TestMatrix <-matrix(6:9,2,2)
#SavedMatrix <-makeCacheMatrix(TestMatrix) #or use# SavedMatrix <-makeCacheMatrix(matrix(6:9,2,2))
#Inv<-cacheSolve(SavedMatrix) #should have message "NOT inversed before, solve matrix on the fly"
#print(Inv)
#Inv<-cacheSolve(SavedMatrix) #should have message "Already inversed, return the result in cache."
#print(Inv)
#print(TestMatrix %*% Inv)


