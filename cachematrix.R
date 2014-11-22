## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix" object that can cache its inverse
##This function will 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## defaults the value of m to NULL
    set <- function(y) { ## sets the value of the matrix
      x <<- y ## stores or caches the inputted matrix which is used to check if inverse has alread been calculated
      m <<- NULL ## sets the value of m
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve ## function which will calculate the inverse of the matrix
    getmatrix <- function() m ## function which will get the value of the matrix
    list(set = set, get = get, ## creates a list of functions
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  }



## Write a short comment describing this function
## CacheSolve function calculates the inverse of the special "matrix" 
## created with the above makeCacheMatrix function. It first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inversr of the data and sets the value
## of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix() ##run the getmatrix function to get the value of the input matrix
  if(!is.null(m)) { ##check to see if cacheSolve has been run before
    message("getting cached data") ## displays the message "getting cached data" if inverse has already been done
    return(m)
  }
  data <- x$get() ## get the value of the matrix and assign it to data
  m <- solve(data, ...) ## apply the solve function which will calculate the inverse of the matrix 
  x$setmatrix(m) ## run the setmatrix function to cache the inverse matrix
  m
}

