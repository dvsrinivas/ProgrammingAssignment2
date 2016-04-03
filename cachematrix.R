## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix and cacheSolve together implement a mechanism to cache inverse of a matrix
## cache is refreshed on demand, if a change is detected in the matrix

## Write a short comment describing this function

## builds a list object with methods to get/ set matrix/ inverse.
## setting the matrix resets the matrix to NULL
## <<- operator is used whenever values are assigned to objects in an env different from calling env
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    getinversion <- function() i
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    setinversion <- function(inver) i <<- inver
    list(get = get, set = set, getinversion = getinversion, setinversion = setinversion)
}


## Write a short comment describing this function

## returns matrix inverse from cache if it is valid (not NULL). Re-computes a new value otherwise.
## New value computed is stored in the cache as well as returned to the caller

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinversion()
    if(!is.null(i)){
        return(i)  
    } 
    a <- x$get()
    i <- solve(a)
    x$setinversion(i)
    i
}