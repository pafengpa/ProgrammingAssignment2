## This function is designed to cache the inverse of a matrix 

## This function creates a special ¡°matrix¡± object that can cache its inverse

makeCacheMatrix<-function(x=matrix()) {
	  ss<-NULL
        set<-function(y) {
                x<<-y
                ss<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) ss<<-inverse
        getInverse<-function() ss
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function computes the inverse of the special ¡°matrix¡± returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve<-function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	  ss<-x$getInverse()
        if (!is.null(ss)) {
                message("getting cached data")
                return(ss)
        }
        pp<-x$get()
        ss<-solve(pp, ...)
        x$setInverse(ss)
        ss
}
