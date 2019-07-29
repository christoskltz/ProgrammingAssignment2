## The aim  is to write a pair of functions, named
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse.
##Following the given example (makevector), the makeCacheMatrix  function is the following:

makeCacheMatrix <- function(x = matrix()) {
    
    inver <- NULL 
    set <- function(y) { 
        x <<- y 
        inver <<- NULL 
    } 
    
    get <- function() x 
    setInver <- function(inver) inver <<- inverse
     
    getInver <- function() inver
    list(set = set, get = get, setInver = setInver, getInver = getInver) 
}                                                                                                                                                                                                           


##This function calculates inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##Following the given example (cachemean), the cacheSolve function is the following:

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    inver <- x$getInver()
    
    if(!is.null(inver)) {
        message("Extracting cached data.")
        return(inver)
    }
    
    my_data <- x$get()
    inver <- solve(my_data, ...)
    x$setInver(inver)
    inver
}   
