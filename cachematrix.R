## I make use of the makeCacheMatrix function to set the value of the matrix and 
## ascribe it to a different environments, for computational efficiency. 

makeCacheMatrix <- function(x = matrix()) {
    
    m<-NULL # sets the value of m to NULL
    
    set<-function(y) { #set the value of the matrix
        x<<-y #caches the inputted matrix so that cacheSolve can check whether it has changed
        m<<-NULL # sets the value of m (the matrix inverse if used cacheSolve) to NULL
    }
    
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    m<-x$getmatrix()
    
    if(!is.null(m)){ # check to see if cacheSolve has been run before
        message("getting cached data")
        return(m)
    }
    
    matrix<-x$get() # run the getmatrix function to get the value of the input matrix
    m<-solve(matrix, ...) # compute the value of the inverse of the input matrix
    x$setmatrix(m)
    m # return the inverse
}

#testing if my code works with the trial variable

trial <- matrix(data = c(13, 23, 10, 7), nrow = 2, ncol = 2)
trial
trial2 <- makeCacheMatrix(mat)
trial2
cacheSolve(trial2)

#here is the output in the console
# > cacheSolve(trial2)
#         [,1] [,2]
#    [1,]  0.6 -0.7
#    [2,] -0.2  0.4

