
## makeCacheMatrix- A special function that holds the below mentioned functions
## 1. set- Function to set the value of the matrix
## 2. get- Function to get the value of the matrix
## 3. setinverse- set the value of the inverse
## 4. getinverse- get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set<- function(y){
        x<<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv.value) inv <<- inv.value
    getinverse <- function() inv
    ## list of functions
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
    
}


## Write a short comment describing this function
## cacheSolve function is used to calculate the inverse of the special matrix. In doing so it first checks to see if the
## inverse of the matrix is calculated, else it calculates the inverse and returns the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinverse()
    if(!is.null(mat)) {
        message("getting cached data")
        return(mat)
    }
    data <- x$get()
    trans <- solve(data,...)
    x$setinverse(trans)
    inv
}
