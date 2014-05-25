##These two functions together serve the purpose of calculating and storing 
## the Inverse of a square matrix into cache.




## This function takes a square matrix as the argument
## and creates a list containing four functions to
## set a matrix, get a matrix, set the value of its inverse 
## and get the value of its inverse 

makeCacheMatrix <- function(x = matrix()) {
	
	Inv<-matrix(nrow=nrow(x), ncol=ncol(x)) ## this intializes a square matrix with NA values

	set<-function(y){
	x<<-y
	Inv<<-matrix(nrow=nrow(x), ncol=ncol(x))
	}
	get <- function() x
	setInv<-function(Inverse) Inv<<-Inverse
	getInv<-function() Inv
	list(set=set, get=get, setInv=setInv, getInv=getInv)
}



## This function takes a "makeCachematrix object" as its argument and provides the Inverse of
## the matrix from Cache, if it already exists. If not, it calculates the 
## inverse and then stores it into the cache.

cacheSolve <- function(x, ...) {

		Inv<-x$getInv()
		if(!is.na(Inv)) { ## this condition checks if the first element of the Inv matrix is an NA
		  message("getting cached data")         
			return(Inv)
  		 }
	  data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}