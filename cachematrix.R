## These two functions cache the inverse of a given invertible matrix 
## (i.e. the matrix which is square and which determinant is different from 0)
## so that the value of the inverted matrix could be accessed from cache 
## instead of being recomputed each time it is needed.
## 'makeCacheMatrix' takes an invertible matrix as an argument. This function essentially
## produces a 'matrix' object used to cache the given matrix's inverse.
## This object can be passed further to the 'cacheSolve' function, which acttually calculates
## the inverse of a given matrix.



## 'makeCacheMatrix' function produces a list, which contains following functions:
## 'set', 'get', 'setinverse' and 'getinverse'
## 1. 'set' function is used to input the value of a matrix, for which an inverse matrix
##  will be computed. You can modify an existing matrix with 'set' -
## when this function is called, it also sets any stored value of an 
## inverse matrix to null.
## 2. 'get' is used to return the the value of the input matrix 
## 3. 'setinverse' sets the value for the computed inverse matrix, so do NOT call it directly
## because it will override the value, computed with the 'cacheSolve' function, described bellow!
## 4. 'getinverse' returns the currently stored inverse matrix, resulted from 'cacheSolve' function

##Example: 
## given matrix m:
## > m <- matrix(c(1,1,1,3,4,3,3,3,4), nrow=3)
## > cm <- makeCacheMatrix(m) 
##
## > cm$get() #returns the input - matrix m
##       [,1] [,2] [,3]
## [1,]    1    3    3
## [2,]    1    4    3
## [3,]    1    3    4
## > cm$getinverse() #if we try to access the inverse of a given matrix at this stage
##                   # we will get an empty object, because to caclulate inverse of m  
##                   # and store this value in the 'cm' object
##                   # we first need to call a function 'cacheSolve' on cm.
## NULL
##

makeCacheMatrix <- function(x = matrix(), ...) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## 'cacheSolve' function checks if the value of an inverse matrix is already stored in cache
## and if it there returns it from cache. Otherewise, inverse of a given matrix is calculated 
## using the 'solve' function and
## then the new value is stored in the object, produced by 'makeCacheMatrix' function.
## (if matrix A is given and B is its inverse, AB = BA = I, where I is an
## identity matrix)
##Example (continuation):
## > cacheSolve(cm) #will return an inverse of a matrix which was input into
##                  # 'makeCacheMatrix' function on the previous step
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1 
## > cacheSolve(cm) #if we call cacheSolve of cm once more, the value will be returned from 
##                  # cache rather then recalculated 
## getting cached data
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1

cacheSolve <- function(x, ...) {
       
    i <- x$getinverse()  
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
