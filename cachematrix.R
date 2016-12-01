## A pair of functions that cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function (x = matrix()) {
        ## Initialize the inverse
        inverse <- NULL  
        ## Set the matrix
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        ## Get the matrix
        get <- function() x
        ## Set the inverse of the matrix
        setinverse <- function(inv) inverse <<- inv
        ## Get the inverse of the matrix
        getinverse <- function() inverse
        ## Return a list of the helper functions
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## Calculate the inverse via matrix multiplication   
        invserse <- solve(data) %*% data
        x$setinverse(inverse)
        ## Return the matrix we need
        inverse
}