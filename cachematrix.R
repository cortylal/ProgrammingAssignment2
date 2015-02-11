## Below are two functions that are used to create a special matrix that 
## stores a matrix and cache's its inverse.


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL           ## Initialization of the cached inverse
        set <- function(y) {            
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() {             ## Get the initial matrix 
                x
        }
        setInverse <- function(inverse) {       ## Sets the cached inverse
                cachedInverse <<- inverse                
        }
        getInverse <- function() {      ## Collects the cached inverse
                cachedInverse
        }
        
        ## Returns a list of the 4 functions 
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve will retrieve the inverse 
## from the cache

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()       ## Collecting the cache data
        if(!is.null(inverse)) {         ## If the inverse was already calculated
                message("The inverse of this matrix was already calculated.\nGetting cached data")
                return(inverse)         ## Returning the cached inverse
        }
        data <- x$get()                 ## Getting the matrix to inverse
        inverse <- solve(data, ...)     ## Calculating the inverse 
        x$setInverse(inverse)           ## Saving the now calculated inverse
        
        ## Returns a matrix that is the inverse of 'x'
        inverse
}
