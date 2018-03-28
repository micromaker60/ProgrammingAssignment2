## makeCacheMatrix creates a list of functions to create and store(cache) a matrix inverse
## cacheSolve solves for the inverse, or else pulls the inverse from the cache


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #function that changes matrix stored in main function
  set <- function(y){
    #resets the main function value x to the new y, hence <<-, <- would only change the value in set()
    x <<- y
    #rests the value of the inverse stored at i to NULL, since there is a new matrix
    i <<- NULL
    
  }
  #get() returns the matrix x stored in main function
  get <- function () x
  #updates i with inverse matrix
  setinverse <- function(inverse) i <<- inverse
  #gets value of i
  getinverse <- function () i
  #stores all the functions in a list so the object assigned to makeCacheMatrix has all the functions
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  #if there is a value stored at i (there is a mean ) then return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #get the matrix value stored at x
  data <- x$get()
  #calculate inverse of matrix
  i <- solve(data, ...)
  #set the new inverse matrix to be cached
  x$setinverse(i)
  i
}
