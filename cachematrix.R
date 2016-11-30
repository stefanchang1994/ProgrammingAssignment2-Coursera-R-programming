makeCacheMatrix <- function (x = matrix()) {
        inverse <- NULL
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        invserse <- solve(data) %*% data
        x$setinverse(inverse)
        inverse
}