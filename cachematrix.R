


### Assignment 2

## There are two functions that are defined here that make use of caching to compute the inverse of a matrix quickly


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        
        
        i <- NULL # Sets the value of inverse to Null
        
        setmatrix <- function(y) 
        {
                
                x <<- y # caches the inputted matrix so that cacheSolve can check whether it has changed.
                i <<- NULL # Sets the inverse to Null
        }
        
        
        getmatrix <- function() x # Returns the matrix 'x'
        
        setinverse <- function(solve) i <<- solve # Sets the inverse i to solve
        getinverse <- function() i # Returns the inverse i
        
        # Returns the special Vector with the functions
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,setinverse = setinverse,getinverse = getinverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not #changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse() # If an inverse has been computed, it gets it.
        
        if(!is.null(i)) 
        {         
                # If the value of inverse i is not null
                
                #Checking if it has already been computed
                
                
                message("getting cached data") # Prints Getting cached data
                
                return(i) # Returns the existing inverse.
        }
        else
        {
                
                data <- x$getmatrix() # Gets the inputted matrix and sets it to data
                
                
                i <- solve(data, ...) # Computes the inverse of the inputted matrix
                
                x$setinverse(i) # Sets the inverse so that it can be cached
                i # returns the inverse
                
        }
}
