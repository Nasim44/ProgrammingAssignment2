## This code is written for Assignment2 for the Rprogramming course for coursera
## functions contains a pair of functions that cache the inverse of a matrix.

## The first function function "makeCacheMatrix" creates a special
## "matrix" object that can cache its inverse. 
## It is a list containing functions to do:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        inv_matrix <- NULL

## this function: "setmatrix" : set the value of the matrix
        setmatrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }

## this function: "getmatrix" : get the value of the matrix
        getmatrix <- function() x


## this function: "setinv" : set the value of the inverse of the matrix
        setinv <- function(inv) inv_matrix <<- inv


## this function: "getinv" : get the value of the inverse of the matrix
        getinv <- function() inv_matrix


## List of the four fuctions
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

##If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
        inv_matrix <- x$getinv()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }

## If the inverse did not calculated , then 
##the cachesolve calculate the inverse.
        data <- x$getmatrix()
        inv_matrix <- solve(data, ...)
        x$setinv(inv_matrix)
        inv_matrix
}
