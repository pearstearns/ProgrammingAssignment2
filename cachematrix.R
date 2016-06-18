## this isn't a function itself so much as a list of functions, kind of like a case of DVDs. 
##you can't put the big blue case in the player and expect it to play, 
##but you can put Speed Racer (2008) dirs. Wachowski Sisters, 
##(bigblue$SpeedRacer)
##into the machine and it will play just fine.
## so basically this has the $set, $get, $setinv and $getinv functions
## you put in a matrix (I use rnorm as well, they need to be square)
## and it puts that in to the (x) variable within the function
## get reads x
## setinv takes x and solves it, placing the inverse matrix in the m variable
## getinv reads m

makeCacheMatrix <- function(x = matrix()) {
## A list of functions to set, retrieve, and invert matrices        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinv <- function(mir) {
                lina <- solve(mir)
                m <<- lina
        }        
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The main function is to return a matric that is the inverse of 'x'. 
## it does this in one of two ways
## 1) cacheSolve checks $getinv to see if there is already an inverse matrix there
## 2) if it isn't it takes the $get value and runs solve() on it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Note: in my environment 'mcm' is "mcm <- makeCacheMatrix()"
        m <- mcm$getinv()
        if(!is.null(m)) {
                message("Here you are, Sir")
                return(m)
        }
        dat <- mcm$get()
        m <- solve(dat)
        mcm$setinv(m)
        m
}
