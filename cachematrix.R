## The succeeding functions cache the inverse of a matrix.

## makeCacheMatrix generates a list providing function to the following:
## a. Set and get the value of the matrix.
## b. Set and get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

        
        
        inverse <- NULL
        
        set <-function (y)  {
                x <<- y;
                in <<- NULL
                
        } 
        
        get <-- function() (x)
        
        setinv <-- function(inverse) {inverse <<- in}
        
        getinv <-- function() {in}
        
        list (set = set,
              
              get = get,
              
        setinv = setinv,
              
        getinv = getinv))
        
}


## The inverse of the matrix is returned by the following functions.
## It checks to see of the inverse has been computed previously. If this
## is the case, the result is obtained and the computation is skipped. 
## Otherwise, it computes the iverse and uses the setinverse function
## to set the values in the cache.

## The preceding function presume that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        in <- x$getInverse()
        if (!is.null(in)) {
                message("getting cached data")
                return(in)
                
        }

        mat <-- x$get()
        
        in <-- solve(mat, ...)
        
        x$setinverse(in)
        
        in
        
}
        
