## These functions invert a matrix only if has not been inverted already.
## If the inverse has already been computed, it is saved in cache and can be 
## loaded from cache

## Create list of functions to set and access a matrix to invert and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #initialize the inverse of x
        set <- function(y) { #function to set the value of the matrix to invert
                x <<- y
                m <<- NULL
        }
        get <- function() x #function to return the matrix we are working with
        setsolve <- function(solve) m <<- solve #function to save the invert in cache 
        getsolve <- function() m #function to get the inverse saved in cache
        list(set = set, get = get, #return a list with the functions which allow to
             setsolve = setsolve,  #access and set the matrix we are working with
             getsolve = getsolve)  
}


## check if the inverse of a matrix has been computed, if so, it loads it from cache
## if not, it computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve() #get the inverse of the matrix 'mat'
        if(!is.null(m)) { #if the inverse has been computed already
                if ( all.equal( x$get() , solve(x$getsolve()) ) ){ #if the matrix has not change
                        message("matrix is still the same")
                        message("getting cached data")
                        return(m) #return the inverse of 'mat'
                }
        }
        data <- x$get() #if the inverse has not been computed, get the matrix to invert
        m <- solve(data, ...) #invert the matrix 'mat'
        x$setsolve(m) #save in cache the inverse of the matrix 'mat'
        m #return the inverse of 'mat'
}
