## Put comments here that give an overall description of what your
## functions do

## This function allows the user to pass in a set of number with the option parameters for the dimensions and converts this
## argument to a matrix with the matrix() function.  For example <-makeCacheMatrix(c(1,5,3, 5,6,5, 5,4,5), nrow=3, ncol=3, byrow=TRUE)


makeCacheMatrix <- function(x=matrix(),...){
        i <- NULL
		x <- matrix(x,...)
        set <- function(y,...) {
                x <<- matrix(y,...)
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
}
