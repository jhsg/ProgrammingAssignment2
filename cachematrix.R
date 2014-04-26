## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##
        ## This function creates a special "matrix" object that can cache its inverse.
        ##
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        
        ## function to set cached inverse value
        setinverse <- function(inverse) inverse <<- inverse
        ## retrive cached inverse value
        getinverse <- function() inverse
        ## form the list of functions 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrive cached value
        m <- x$getinverse()
        
        ## if the retrived value is not NULL, return the value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get matrix
        data <- x$get()
        ## calculate inverse..
        m <- solve(data, ...)
        
        ## store value in cache.. 
        x$setinverse(m)
        
        ## show the value
        m
}
