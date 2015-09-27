## The below two functions are used in conjucntion with each other 
## to compute the inverse of a matrix and cache the result.
## Matrix inversion computation intensive process and the idea
## behind these to fuctions is to compute the inverse once and use
## reuse the result when required again when similar matrix inversion 
## is required.

## makeCacheMatrix() creates a special "vector" which is really a list
## containing the below functions each as a element in the list
## [1] set the matrix
## [2] get the matrix
## [3] set the inverse of the matrix
## [4] get the inverse of the matrix
##
## funcation Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
        # Creating a makeCacheMatrix object will consist of four 
        # functions in a list and takes a invertible matrix as input
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse of the matrix
        # 4. get the inverse of the matrix
        
        # Initially set to NULL
        # Changes when the user sets the value
        inv <- NULL
        
        # set function
        # Sets the matrix itself but not the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get function
        # Gets the matrix itself but not the inverse
        get <- function() x
        
        # Manually set the inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get the inverse
        getinverse <- function() inv
        
        # Encapsulate into a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)	
}

## cacheSolve() computes the inverse of the special "vector" 
## created with the above makeCacheMatrix() function. 
## It first checks to see if the inverse has already been computed.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse of the matrix and sets 
## the value of the inv in the cache via the setinverse function.
##
## function Usage example:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## res <- cacheSolve(m)
## print(res)
## res should return:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## res2 <- cacheSolve(m)
## This should display a "Getting cached matrix" message
## print(res2)
## res2 should return
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the current value of the inv from cache
        inv <- x$getinverse()
        
        # If inv has a value set
        if(!is.null(inv)) {
                # return the previously computed inverse		
                message("Getting cached matrix")
                return(inv)
        }
        
        # If inv has not been set
        # Get the matrix itself
        data <- x$get()
        
        # Find the inverse
        inv <- solve(data, ...)
        
        # Cache this result in the object
        x$setinverse(inv)
        
        # Return this new result
        inv    
}