##My transformation: In the sample code I have replaced m with s. 
##As input I have placed a matrix (x). I have assigned s = NULL.
##In the rest of the code I have changed the mean function for the solve function.
##In makeCacheMatrix, for a given matrix (generated in some way) a list 
##is established that generates and contains the matrix, and generates and 
##contains its inverse (in our case, for the test example, the matrix was 
##generated with standardized normal random numbers, is square of order three)


makeCacheMatrix <- function(x = matrix()) {
 s <- NULL
  set <- function(y){
  x <<- y
  s <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) s <<- inverse
  getInverse <- function() s 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}


##The cacheSolve code returns the inverse of the generated matrix, if it has 
##not been calculated before. If the inverse value has already been calculated,
##then it returns that cached value.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
  if(!is.null(s)){
  message("getting cached data")
  return(s)
  }
  mat <- x$get()
  s <- solve(mat,...)
  x$setInverse(s)
  s

}
