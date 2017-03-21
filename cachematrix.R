## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}


#Test run 1
                
#Create matrix
#myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
                
#Print matrix
#myMatrix$get()
                
#getInv is null because inverse matrix is not yet created
#myMatrix$getInv()
                
#Create inverse matrix
#cacheSolve(myMatrix)
                
#Retrieve cached data
#myMatrix$getInv()

#Test run 2
                
#Create matrix
#myMatrix$set(matrix(c(4, 3, 2, 1), 2, 2))
                
#Print matrix
#myMatrix$get()
                
#getInv is null because inverse matrix is not yet created
#myMatrix$getInv()
                
#Create inverse matrix
#cacheSolve(myMatrix)
                
#Retrieve cached data
#cacheSolve(myMatrix)
