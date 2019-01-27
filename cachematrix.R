# While having to perform time consuming operations repetitively, it makes sense
# to cache the results and fetch them on demand rather than repeating the entire 
# computation. Here is a demonstration of how this gets done with a matrix inverse
# opration. The function pair below work in tandem. The first one takes
# in a matrix and sets it within an object that holds not just the data but (optionally)
# also its inverse. This state persists between function calls. If the inverse 
# has already been computed, it can simply be accessed the next time, instead of
# computing it again. 

## The function makeCacheMatrix returns a list with functions written to set/get
##  the value of a matrix and set/get its inverse.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## cacheSolve returns a matrix that is the inverse of 'x'. It computes the
## inverse if it has not already been computed. Otherwise, it just fetches the
## cached inverse matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("Fetching cached inverse ...")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
