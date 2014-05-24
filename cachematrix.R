## The two functions below--makeCacheMatrix and cacheSolve--create a special "matrix" object that stores 
## a matrix and caches the inverse of the stored matrix.

## The function, makeCacheMatrix, creates a special "matrix" object that can cache the inverse matrix of 
## the function's argument by setting the matrix, getting the matrix, setting the inverse of the matrix, 
## and getting the inverse of the matrix. The special "matrix" object is actually a list of 4 functions:
## set, get, setinvers and get inverse.

makeCacheMatrix <- function(x=matrix(data=NA, nrow=NA, ncol=NA)) {
  
    m <- NULL   ##set null object m
    ## define set function:
    set <- function(y) {
    x <<- y   ## y is assigned to the value of x in the parent environment
    m <<- NULL  ## 
}
    ## define get function; since argument is empty, the get function returns x
    get <- function() x

    ## define setinverse function. setinverse function assigns in parent environment its argument 'inverse' to m
    setinverse <- function(inverse) m <<- inverse

    ##  define getinverse function. getinverse function has no argument; getinverse function returns m
    getinverse <- function() m

    ## return list of of the functions set, get, setinverse, getinverse
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##---------------------
# The cacheSolve function takes as input the "matrix" object that was created by the function makeCacheMatrix
# and checks to see if the inverse of the input to makeCacheMatrix has been calculated. If the matrix inverse has 
# been calculated, cacheSolve gets the inverse from cache. If the matrix inverse has not been calculated, 
# cacheSolve calculates the matrix inverse.

cacheSolve <- function(mlist, ...) {
  ## note: mlist is a list of 4 elements, namely the functions defined in the makeCacheMatrix function: set, get, 
  ## setinverse, and getinverse
  
  ## assign to m the output of the getinverse element of list mlist when argument of getinverse is empty
  m <- mlist$getinverse()
  
  ## check to see if matrix inverse is not NULL. If not NULL, return the message "getting cached data" and return the value m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Otherwise, assign to 'data' the output of the get function (from mlist ) with empty argument
  data <- mlist$get()
  
  ## assign to m, the inverse of data
  m <- solve(data, ...)
  
  ## call the setinverse funtion from mlist with input m
  mlist$setinverse(m)
  
  ##return m
  m
}
##*** For Pam ********************************************************
##Sample run of functions. To run, remove single #s.
# x <- matrix(1:4,2,2)  ## define matrix x (has to be invertible)
# x   ##return matrix x
# makeCacheMatrix(x)   
##call makeCacheMatrix function on input x; this produces list of the 4 functions defined in the makeCacheMatrix function
# mlist<-makeCacheMatrix(x) ##assign to mlist the output of the makeCacheMatrix function with input of x
# cacheSolve(mlist)  ##checks to see if inverse of x has been cached. If so, returns it; if not, calculates it.
# x %*% cacheSolve(makeCacheMatrix(x))  ##should return identity matrix
##********************************************************************


