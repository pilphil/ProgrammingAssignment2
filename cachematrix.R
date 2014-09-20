## The pair of functions below, perform actions such as getting 
## and setting elements of the matrix, and setting the inverse
## of the matrix

## makeCacheMatrix takes a matrix as input and returns an object with 
## methods to perform actions such as getting and setting elements
## of the input matrix and also get and save the inverse of the input 
## matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## Update the value of x and inverse of x.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## return elements of input matrix x
        get <- function() x
        
        ## store the inverse
        setinverse <- function(seti) {
                inverse <<- seti
        }
        
        ## return inverse 
        getinverse <- function() inverse
        
        ## matrix object's methods
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix of the object created by
## the makeCacheMatrix function. If there is an existing inverse, this
## is cached value is returned and no calculation is performed. If the
## cached value is NULL, then solve() is called to calculate the inverse
## and store the value using the setinverse() method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                ##return cached value of the inverse
                return(i)
        }
        
        ## There is no cached value for the matrix's inverse
        ## To calculate inverse, first get elements of the matrix
        mat <- x$get()
        
        ## calculate its inverse
        i <- solve(mat)
        
        ## Store this value
        x$setinverse(i)
        i
}
