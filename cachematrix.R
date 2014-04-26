## This function is used to stored the actual matrix for which inverse matrix vlaue need to be 
## cached and also the required function to set and get the matrix and set and get functions 
## to save the value to the cache and retrive the cache value

## This function takes the matrix as the input parameter for which inverse matrix 
## need to be calculated and cached.
## This function creates and returns a list. Each member of a list is a funtion. 
## List returned contains set,get,setInverse and getInverse functions  

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse value of the the matrix x which is cached for later use 
  setInverse <- function(invertedMatrix) m <<- invertedMatrix
  
  ## get the inverse value of the the matrix
  getInverse <- function() m
  
  ## funtion retruning list that contains set,get,setInverse and getInverse functions  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## The input is expecting a "special vector" made from makeCacheMatrix.
## The output is the matrix inverse coming whether from the special vector's  cache or computation.

cacheSolve <- function(x, ...) {
        
  #query the x matrix's cache
  m <- x$getInverse() 
  
  #if there is a cache
  if(!is.null(m)) { 
    
    message("getting cached data")
    
    #just return the cache, no computation needed
    return(m)  
  }
  
  #if there's no cache get the matrix for which inverse matrix need to be created
  data <- x$get()  
  
  # calculate inverse of the matrix
  m <- solve(data) 
  
  # save inverse of the matrix to cache
  x$setInverse(m)
  
  m  
  
}
