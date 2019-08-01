## The aim  is to write a pair of functions, named
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix


##This function creates a special "matrix" object that can cache its inverse.
##Following the given example (makevector), the makeCacheMatrix  function is the following:


makeCacheMatrix <- function(x = matrix()) { ## argument definition  with  "matrix" default mode of
    
    inverse <- NULL             ##initialising
    set <- function(y) {        ##defining set function
        x <<- y 
        inverse <<- NULL        ##reset inverse in case of new matrix
    } 
    
    get <- function() x         ##defining get function
    set_inverse <- function(inverse) inverse <<- inverse2 ##assigning to parent environment
     
    get_inverse <- function() inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse) 
}                                                                                                                                                                                                           



##This function calculates inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Following the given example (cachemean), the cacheSolve function is the following:

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$get_inverse()
    
    if(!is.null(inverse)) {
        message("Extracting cached data")
        return(inverse)
    }
    
    my_data <- x$get() 
    inverse <- solve(my_data, ...) ## returning the inverse
    x$set_inverse(inverse2)
    inverse2
    
}
