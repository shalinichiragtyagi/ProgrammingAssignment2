## The use of the function below is to cache the inverse of a matrix: 
## Generally matrix inversion is understood to be a lengthy and costly computation. 
## There are some benefit of caching the inverse of a matrix rather than compute it repeatedly for the same matrix.
## A pair of functions are given below which is designed to create a special object that can
## store the matrix and caches its inverse.

makeCacheMatrix <- function (x= matrix()){
  i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) i <<- inverse
  getInv <- function() i
  list (set =set, get=get,
        setInv=setInv,
        getInv=getInv)
}


## The following function can be used to compute the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and provided the
## matrix has not changed, then it should retrieve the inverse from the stored cache.

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  h <- x$get()
  i <- solve(h, ...)
  x$setInv(i)
  i
}