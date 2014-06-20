## Put comments here that give an overall description of what your
## functions do
## This set of functions will take an invertable matrix and cache it.
## the function makeCacheMatrix() can either initialize an empty special matrix
## or can accept a matrix as an argument and cache it as a special matrix.
## 
## The first time an inverse is retrieved using cacheSolve() 
## solve() will be called and the result cached. The next time cacheSolve()
## it called it will retrieve the cached result.
## 
## example : intitialize our cacheable matrix 
## m <- makeCacheMatrix()
## create an invertible matrix
## b <- matrix(c(0,2,3,5,1,7,1,2,4), nrow = 3, ncol = 3)
## then store the store the matrix using 
## m$set(b)
## the first call to cacheSolve(m) will run the solve() function
## and display and cache the result.
## > cacheSolve(m)
## [,1] [,2] [,3]
## [1,]  -10  -13    9
## [2,]   -2   -3    2
## [3,]   11   15  -10
## 
## The second time it will get the cached result
## > cacheSolve(m)
## getting cached data
## [,1] [,2] [,3]
## [1,]  -10  -13    9
## [2,]   -2   -3    2
## [3,]   11   15  -10

## Write a short comment describing this function
## Write cache matix can initialize an empty special matrix or accept a 
## invertible matris as an agument and will cache it.
## this fuction produces a matrix with 4 methods 
## 1. set() caches a matirx
## 2. get() outputs the cached matrix
## 3. getinverted() Gets the inverted matrix from cache
## 4. setinverted() calls solve() and caches it.

makeCacheMatrix <- function(x = matrix()) {
  # x is an empty or invertible matrix
  if (!is.na(x)) { #if x is a real matrix lets cache it
    x <<- x
  }
  # initialize our cached inverted 
  invx <<- NULL
  
  set <- function(m) {
    x <<- m # cache the matrix
    invx <<- NULL #reset the inverted cache to null
  }
  
  get <- function() {
    x # retrive the cached matirx
  }
  
  # set the inverted matrix to cache
  # chose to run solve in setinverted from in the call below
  # it make for a more cleaner and understandable 1 call solution in cachesolve function
  # and give exteneded functionality to this function
  
  setinverted <- function(solvedi, ...) {
    invx <<- solve(solvedi, ...) # cache it !
    return(invx) # return inverted matrix
  }
  
  # get the inverted matrix from cache
  getinverted <- function() { 
    invx 
  }
  
  # return our fuctions list
  list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
  
}


## Write a short comment describing this function
## cacheSolve take a special matrix as an argument
## returns inverted matrix. trys cache first if it finds
## the matrix it returns it if not it puts in to cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get the inverse of the matrix from cache       
  inverted <- x$getinverted()
  if (is.na(x$get()[[1]])) {
    message("empty special matrix !")
    return(NA)
  }
  
  # check if there is the inverted matrix in our cache  
  if(!is.null(inverted)) {
    message("getting cached data")
    return(inverted)
  } else {
    # if we didn't get a cache hit   
    # place the inverse of the matrix in cache and retreive it
    # chose to run solve in setinverted from the call below
    # it make for a more cleaner and understandable 1 call solution
    # and give exteneded functionality to makecache
    inverted <- x$setinverted(x$get(), ...)
    return(inverted)
  }
}
