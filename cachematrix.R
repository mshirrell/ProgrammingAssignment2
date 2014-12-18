
## This R script contains code for two functions, (1) makeCacheMatrix and 
## (2) cacheSolve. See below for descriptions of each function.     

##      (1) makeCacheMatrix: This function creates a special "matrix" 
##      object that can cache its inverse. This function takes a matrix
##      as its input

makeCacheMatrix <- function(x = matrix()) {
        ## assign the object "m" a null value within the function: 
        m <- NULL
        ## create an object called "set", which contains a function allowing 
        ## the user to assign different values to the matrix x, and which 
        ## reassigns a null value to the object "m" within the global 
        ## environment: 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## have the object "get" simply return the matrix x:
        get <- function(return) x
        ## have the object setinverse set the inverse of the matrix x:
        setinverse <- function(solve) m <<- solve
        ## have the object getinverse return the inverse of the matrix x:
        getinverse <- function(solve) m
        ## put each of the above objects into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##      (2) cacheSolve: This function computes the inverse of the special 
##      "matrix" computed by makeCacheMatrix above. If the inverse has already
##      been calculated, then this function retrieves the inverse from the
##      cache.

cacheSolve <- function(x, ...) {
        ## look for the value of "getinverse" computed above (the inverse of 
        ## the matrix x), and assign it to the object "m":
        m <- x$getinverse()
        ## if there is a previously-computed value for the inverse, display
        ## that value:
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if there is no previously-computed value for the inverse, get
        ## the matrix x and assign it to the object "data":
        data <- x$get()
        ## compute the inverse of the matrix and assign it to the object "m"
        m <- solve(data, ...)
        ## set the inverse of the matrix x
        x$setinverse(m)
        ## display the inverse of the matrix x
        m
}