## This code consists of two functions, the first one to create a matrix with
## four functions (setting data, getting data, setting the inverse, and getting it)
## The second function will cach the inverse of this matrix and get it back.
## This code is written for Programming Assignment 2 - R programming course on Coursera
## 27-04-2014

## This function creates an object which stores matrix data ($set),
## retrieves it back ($get), sets the inverse of this matrix ($setInverse),
## and retrieves it back ($getInverse).

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {   ## This method sets the matrix data, and resets the inverse to NULL
		x <<- y
		i <<- NULL
	}
	get <- function() x   ##This method only returns back the matrix values
	setInverse <- function(inverse) i <<- inverse   ##This method only sets the inverse matrix
	getInverse <- function() i    ##This method only retrieves the inverse matrix
	list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## This function checks whether the given matrix object (the special object created
## by the function makeCacheMatrix above) contains the inverse matrix stored in it or not
## and retrieves it if it is already cached (stored). If it is not, it calculates the inverse
## matrix, sets it (caches it) inside that object, and prints it back.

cacheSolve <- function(x, ...) {
	  i <- x$getInverse()
	  if(!is.null(i)){   ##Checks if there is already a cached inverted matrix, and prints it
		message("getting cached data")
		return(i)
	  }
	  ##If the inverted matrix is not available (not cached, i.e. NULL), the
	  ##following code gets the original matrix data, stores its inverse in the variable i
	  ##and the sets this inverse in the matrix cache and prints it to the user
	  data <- x$get()   
	  i <- solve(data, ...)
	  x$setInverse(i)
	  i
}
