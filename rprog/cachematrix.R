## Provides two functions to make a matrix with the ability to cache and 
##   retrieve its inverse

##Example of use:
##  > emmy <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
##  > cacheSolve(emmy)
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
##  > cacheSolve(emmy)
##  using cached inverse
##       [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## only first use of cacheSolve function will actually calculate the inverse
## all subsequent uses will retrieve cached inverse until matrix value is
## changed


## makeCacheMatrix creates a list object containing four functions
## inputs - x = an invertable matrix
## outputs - a list that object acts as a matrix-like object, contains:
##   set : can be used to set a new value of matrix after its creation
##   get : can be used to get the current value of the matrix
##   setinv : used to set the cached inverse value of the matrix
##   getinv : used to get the cached inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() {x}
    setinv <- function(x_inv) {inv <<- x_inv}
    getinv <- function() {inv}
    #return list which acts as new matrix object with cache-able inverse
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of matrix made with the makeCacheMatrix
## function. When possible it will return a cached value instead of resolving
## inputs - x = a "matrix" (list) that has been made with makeCacheMatrix
##			... = additional inputs to solve function used to calc inverse
## outpus - matrix object of inverse of x

cacheSolve <- function(x, ...) {
    if (!is.null(x$getinv())) {
        message('using cached inverse')
        return(x$getinv())
    } else {
        x$setinv(solve(x$get(), ...))
        x$getinv()
    }
}
