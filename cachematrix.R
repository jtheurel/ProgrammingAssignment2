## create a special "matrix" object that can cache its inverse
##last submission testing SHA
##set and get to apply to matrix
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
	set <- function(y) {     
		x <<- y  ##an enviroment variable differete from the current
		inv <<- NULL
	}##function(Y)

	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computes the inverse of the special "matrix" returned by the function above
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

	if(!is.null(inv)){  ##this happend when the data has been already computed
		message("Getting Cached Data")
		return(inv)
	}##if
	matr <- x$get()
	inv <- solve(matr, ...)   ##function solve
	x$setinv(inv)
	return(inv)
}##function
