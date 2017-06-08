#################################################################################################
#       Topic: Peer-graded Assignment: Programming Assignment 2: Lexical Scoping                #
#       Submitted by: Nitya Tiwari                                                              #
#       Course: Programming with R                                                              #
#       Date submitted: June 8th, 2017                                                          #
#       Objective: This program mainly tells how to use the scoping feature of R language.	    #
#       As an example it fetches the inverse of a matrix from the cache if its been calculated  #
#       and stored in cache earlier, else it calculated it and pushes it to cache for next time.#
#################################################################################################

## This function takes a n by n matrix and returns a list having 4 elements where each of the list element is a
## function, get retuns the matrix, getInverse returns the matrix inverse or NULL. Other two functions are used
## to set the value of the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # initializing inverse of matrix
    inverse <- NULL
    
    # insert matrix in the cache & set inverse to NULL as data is being changed here
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # retrieve matrix from the cache
    get <- function() x
    
    # insert inverse in the cache 
    setInverse <- function(i) inverse <<- i
    
    # retrieve inverse from the cache
    getInverse <- function() inverse
    
    # return a list having 4 elements each of which is a function described above
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function retrieves matrix inverse from the cache and returns it. If it doesn't find it in the cache,
##  it retrieves the matrix from the cache, calculates its inverse and returns it.
cacheSolve <- function(x, ...) {
    # retrieve inverse from the cache
    inverse <- x$getInverse()
    
    # if inverse is NULL, calculate it after fetching matrix from the cache, then insert inverse in the cache
    if(is.null(inverse)) {
        m <- x$get()
        inverse <- solve(m, ...)
        x$setInverse(inverse)
    }
    else {
        print("getting matrix inverse from cache...")
    }
    ## Return inverse of matrix 'x'
    inverse
}
