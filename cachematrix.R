## The makeCacheMatrix has been built by looking at the assignment2 example function makeVector
## The  function, makeCacheMatrix creates a  a list containing a function to
##set the value of the matrix,get the value of the vector,set the inverse of the matrix, the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
   set <- function(y) {
     x <<- y
     s <<- NULL
   }
  get <- function() x
  setInv <- function(solve) s <<- solve
  getInv <- function() s
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## solve function will be used to return the matrix inverse
        
        ## first check if the incoming Cache Matrix has the inverse stored
        cache_val <- x$getInv()
   ##   if the inverse is found, return the cached value 
    if(!is.null(cache_val)) {
      return(cache_val)
   }
   
   ##   if the inverse is not  found, compute the inverse using solve and then set the Inverse
  dataMatrix <- x$get()
  cache <- solve(dataMatrix, ...)
  x$setInv(cache_val)
  cache_val
      
}
 
 
