## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #create matrix object
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL   #set the value of the matrix in parent environment
    }
    get <- function() x  #get the value of the matrix
    setsolve <- function(solve) m <<- solve  #set the value of the inverse of the matrix
    getsolve <- function() m  #get the value of the inverse of the matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)){   #checks to see if m has an assigned value
        message("getting cached data")  #retrieves the inverse from the cache
        return(m)  #return the inverse of matrix x
    }
    data <- x$get()  #get the value of the matrix
    m <- solve(data, ...)  #calculate the inverse of the matrix
    x$setsolve(m) #set the value of the inverse in cache
    m  #return the inverse of matrix x
}