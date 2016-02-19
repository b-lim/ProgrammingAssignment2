## Create matrix object and cache its correlation matrix


## This function takes a matrix argument with form "matrix(numeric vector, nrow=n1, ncol=n2")
## , then stores the following in a list 
## (1) gets and sets the matrix value. 
## (2) It also gets and sets the inverted matrix value

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
				x <<- y
				m <<- NULL 
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set=set, get=get,
			setinverse=setinverse,
			getinverse=getinverse)
}


## Check if inverse has been calculated, if yes, 
## get inverse from cache
## else set inverse via setinverse function
## To compute, Store the previous function in a variable (e.g. j variable) and pass that variable
## on to cacheSolve(j) OR cacheSolve(makeCacheMatrix(x))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m<-x$getinverse()
		if(!is.null(m)){
				message("getting cached data")
				return(m)
		}
		data <- x$get()
		m<-solve(data,...)
		x$setinverse(m)
		m
}
