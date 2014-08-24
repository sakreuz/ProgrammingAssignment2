## Programming Assignment 2 - Stacy Kreuziger
## Requirement: write an R function able to cache potentially time-consuming
## computation. 
## These functions create an object to hold a matrix and then cache the
## inverse of that matrix.

## This function creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse property to null
        inv_x <- NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        
        ## Get the matrix
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse<- function(inverse) inv_x <<-inverse
        
        ## Get the inverse of the matrix
        getinverse <- function() inv_x
        
        ## List the methods for setting and getting the matrix and inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns the inverse of a matrix created with the above
## makeCacheMatrix function.  If the cached inverse is already available,
## it will be retrieved. If not, it will be computed, cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()
        
        ## If the inverse is already set, just return it
        if (!is.null(inv_x)) {
                message("Getting the cached inverse matrix")
                return(inv_x)
        } 
        
        ## Otherwise get the matrix from the special object
        else {
                ##calculate the inverse
                inv_x <- solve(x$get())
                ##set the inverse to the object
                x$setinverse(inv_x)
                ## return the inverse matrix
                return(inv_x)
        }
}

