## These functions introduce a special matrix object which caches its inverse.
## The create such a special matrix inv you execute \code{inv <- makeCacheMatrix(x)}
## where x is an ordinary matrix. You can then get the value with \code{inv$get()}
## and change the value with \code{inv$set(y)} where y is an ordinary matrix.
## You can get the inverse with \code{cacheSolve(inv)}.

## makeCacheMatrix function does the following:
# a. Store the cached inverse matrix in inv 
# b. Set the value of the matrix
# b. Get the value of the matrix
# c. Set the value of the inverse
# d. Get the value of the inverse
# e. Return the matrix with our newly defined functions

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL  # inv will store the cached inverse matrix
        
        # Define function to set the value of the matrix. 
        #It also clears the old inverse from the cache.
        
        set <- function(y) {
                x <<- y         # Set the value
                inv <<- NULL    # Clear the cache
        }
        
        # Define function to get the value of the matrix
        get <- function() x
        
        # Define function to set the inverse. This is only used by getInverse() 
        #when there is no cached inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # Define function to get the inverse
        getInverse <- function() inv
        
        # Return a list with the above four functions
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve function does the following:
# a. Compute the inverse of the matrix if the inverse not already computed. 
# b. If the inverse is already computed before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse() # This fetches the cached value for the inverse
        if(!is.null(inv)) { # If the cache was not empty, we can just return it
                message("getting cached data.")
                return(inv)
        }
        
        #  If the cache was empty,We need to calculate it, cache it, and then return it.
        data <- x$get()         # Get value of matrix
        inv <- solve(data)      # Calculate inverse
        x$setInverse(inv)       # Cache the result
        inv                     # Return the inverse
}



# Example test:
# > x <- matrix(rnorm(4), nrow = 2)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse
