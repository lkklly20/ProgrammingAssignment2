## Matrix inversion - pair of functions that cache the inverse of a matrix

## creating matrix that cache/save its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## initializing object x & m
    m <- NULL                ## m used for later        
    ## behavior of the function
    set <- function(y) {     ## creating vector
        x <<- y              ## using form assignment operator assigning value to
        m <<- NULL           ## right side of object to parent environment 
    }
    get <- function() x      ## x outside of function, gets fromo parent environment
    setinverse <- function(inv) m <<- inverse 
    getinverse <- function() m
    list(set = set, get = get,
         setmean = setmean,      #naming setmean()
         getmean = getmean)      #naming getmean()
}


## computing the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()       #get inverse from what is passed to m
    if(!is.null(m)) {         #if false - gets vector from input
        message("getting cached data")
        return(m)
    }
    data <-x$gt()
    m <- solve(data, ...)    #to get inverse, solve()
    x$setinverse(m)
    m
}
