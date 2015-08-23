# Programming Assignment 2: Lexical Scoping

##This function creates a special "matrix" m object that can cache its inverse as follows
### 1) set the value of m
### 2) get the value of m
### 3) set the value of the inverse of m
### 4) get the value of the inverse of m
makeCacheMatrix <- function(x = matrix()) {
	inversa <- NULL
	set <- function(y){
		x <<- y
		inversa <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inversa <<- inverse
	getinverse <- function() inversa
	list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)	
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
###If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inversa <- x$getinverse()
    if(!is.null(inversa)) {
		message("getting cached data")
		return(inversa)
        }
	data <- x$get()
	inversa <- solve(data)
	x$setinverse(inversa)
	inversa
}
