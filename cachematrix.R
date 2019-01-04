## Functions below return the inverse of the given 2x2 matrix.
## If inverse of given matrix is already cached, cached inverse values are returned; otherwise,
## inverse of given matrix is calculated, cached and returned.

#makeCacheMatrix
##Function takes in a 2x2 matrix and creates additional functions to set and retrieve matrix values
##and the inverse of matrix.  R, taking advantage of the lexical scoping property, allow data
##be set/saved to a special environment where subsequent call to this function
##will have access to not only list of functions defined here but saved data referenced as well.

##Note:  since discussion forum confirms assignment function is to take in a square matrix
##2x2 matrix with default values is created as argument
##see line comments in demonstrating how I created this function

makeCacheMatrix <- function(x = matrix(c(2,4,6,8),c(2,2))) {
        cinv <- NULL 
        set <- function(y) {    
                x <<- y  ##caches y argument and set to special environment via <<-
                cinv <<- NULL  ##clears out previous inverted values since a new matrix is being set
        }
        get <- function() x ##gets cached value, it is like function(){x} returning just x
        setinv <- function(sinv) cinv <<- sinv ##saves solved inversion to special envrionment via <<-
        getinv <- function() cinv ##gets cached value, it is like function(){cinv} returning just cinv
        list(set = set, get = get, ##returns list of functions
             setinv = setinv,
             getinv = getinv)
}


##cacheSolve
##Function takes in results of makeCacheMatrix and returns inverse of matrix retrieved.
##Specifically, if matrix retrieved already has inversed values cached, cached inverse values 
##will be returned.  Otherwise, inverse of matrix will be calculated, cached and returned.

##see in line comments in demonstrating how I created the function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv() ##gets cached inverse via passed in function makeCacheMatrix, and assigned to inv
        if(!is.null(inv)) { ##if cached inverse is not null, returns cached inverse
                message("getting cached data")
                return(inv)
        }
        data <- x$get() ##out of if statement, gets matrix values via passed in function makeCacheMatrix, assigned to data
        inv <- solve(data, ...) ##calculates inverse of data
        x$setinv(inv) ##caches newly calculated inverse value via passed in function makeCacheMatrix
        inv ##returns newly calculated inverse values
}
