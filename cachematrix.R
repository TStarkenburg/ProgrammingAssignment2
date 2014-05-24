## These functions make it possible to save costs by not having to recompute
## inverse of a large matrix but saving it in the cache and only recomputing it
## when neccesary.

## This function creates a special matrix that includes functions 
## to get and set the matrix as well as the inverse of the matrix
## so that they can be saved in cache.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## the inverse of the matrix
 
        ## functions to set and get the matrix itself 
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
 
        ## functions to set and get the inverse
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
     
        ## combine the above functions in a list for easy access to matrix x 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the special matrix 'x'. 
## If the inverse is already cached it returns that, otherwise the inverse is computed.

cacheSolve <- function(x, ...) {
    ## First check if the inverse is already caculated for x    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## the inverse is already in the cache
        message("getting cached data")
        return(inv)
    }
    ## the inverse is not cached and has to be computed and saved in cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

