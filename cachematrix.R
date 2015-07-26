## The functions calculate and cache the inverse matrix
## If inverse matrix is available in cache, return cache value without
## time-consuming recalculation. If not, calculates inverse matrix,
## stores in cache, and returns value.

## MakeCacheMatrix - create a special "Matrix" which is a list containing 4 functions
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL   ## reset n to NULL
  
  ## function1. set - set value of a matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## fucntion2. get - get value of a matrix
  get <- function(){ x }
  
  ## function3. setInv - set value of inverse matrix
  setInv <- function(inv){ n <<- inv }
  
  ## function4. getInv - get value of inverse matrix
  getInv<- function(){ n }
  
  ## return value which is a list containing 4 functions above
  ## these functions are used in 'CacheSolve'
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## CacheSolve - calculate and return an inverse matrix
## First check if inverse matrix has been calculated and cached 
## If so, get and return inverse matrix from cache
## If not, calculate inverse matrix using Solve(), set inverse matrix to cache and return  
cacheSolve <- function(x, ...) {
  
  n <- x$getInv()       ## get inverse matrix from cache
  
  ## check if the inverse matrix "n" retrived is available (Not Null)
  ## if not null, then return cached value and exit function
  if(!is.null(n)){ 
    message("Getting cached data")
    return(n)
  }
  
  ## if cache value is null skip if clause & continue below
  data <- x$get()       ## getting matrix
  n <- solve(data,...)  ## calculate inverse matrix
  x$setInv(n)           ## store inverse matrix into cache
  n                     ## return inverse matrix value
}
