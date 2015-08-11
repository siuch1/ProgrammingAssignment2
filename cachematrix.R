## Function makeCacheMatrix initializes a matrix stored in Global Environment.
## a <- makeCacheMatrix(x=matrix(1:4, nrow=2, ncol=2))
## a$get()                                              ## retrieve matrix a
## b <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3)    ## new matrix b
## a$set(b)                                             ## original a is replaced by new b
## c <- solve(a$get())                                  ## c is Inverse of a
## a$setInv(c)                                          ## to store c in global environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(solve) m <<- solve
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## cacheSolve(a)                
## Solve for the inverse of matrix "a" real-time if no cached data
## Retrieve "m" if "m" has the cached data
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m        
}

## Codes for demonstrations
a <- makeCacheMatrix(x=matrix(1:4, nrow=2, ncol=2))
b <- matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, ncol=3)
a$get()
a$set(b)
a$get()

c <- solve(a$get())
a$setInv(c)
a$getInv()

d <- matrix(c(1,2,2,1), nrow=2, ncol=2)
a$set(d)
e <- solve(a$get())
a$setInv(e)
a$getInv()
a$get()

cacheSolve(a)
