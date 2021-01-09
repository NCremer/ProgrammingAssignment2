## The function cacheSolve checks whether you have the inverse of a matrix
## already stored in the cache, if that is not the case, then it uses the
## makeCacheMatrix function to save the inverse for future reference

## See the comments in the code itself for clarifications

makeCacheMatrix <- function(origMat = matrix()) {
	inv <<- NULL
	## first two lines define your original matrix and set the
	## inverse to NULL

	## the set function allows you to set new values for the matrix
	## and remove the cached inverse
	set <- function(y) {
		origMat <<- y
		inv <<- NULL
	}
	## allows you to get the original matrix in the cacheSolve function
	get <- function() origMat
	
	## allows you to save the calculated inversed matrix in the 
	## parent environment for later retrieval
	setinv <- function(calcInv) inv <<- calcInv

	## allows you to get the cached inverse
	getinv <- function() inv

	## allows you to call all four functions by name
	list(set = set, get = get, setinv = setinv, getinv = getinv)
	
}


## See comments in the function itself for clarifications

cacheSolve <- function(MatrixObject, ...) {
	## gets the inverse value and returns if not NULL
	inv <- MatrixObject$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	## continues here if the above inverse value was NULL
	## gets the matrix, calculates the inverse, saves it and returns it
	data <- MatrixObject$get()
	inv <- solve(data)
	MatrixObject$setinv(inv)
	inv
}

