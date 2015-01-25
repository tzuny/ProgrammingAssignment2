## These two functions generate a matrix and calculate the 
## inverse, by checking first if the inverse has been 
## calculated


## The function makeChaceMatrix generates a list of functions
## that we apply to a matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## the function cacheSolve calculates the inverse o the 
## matrix generated vy makeCacheMatrix. To save time, it 
##first verifies if the inverse has already been calculated
##if it is, the inverse is return from cache, otherwise is 
##calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){  ##checking if inverse has been calculated
        message("returning the inverse matrix from cache")
        return(m)
    }
    ## otherwise we get the matrix, calculate de inverse and we store it in the cache
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m ## returning the inverse matrix
}
