## #makeCacheMatrix function makes a list of of methods with get and set values and inversing matrix 

makeCacheMatrix <- function(x = matrix()) {
     inVse1 <- NULL #intialises the inverse


        set <- function(userMatrix) 
        {
                x <<- userMatrix
                inVse1 <<- NULL
        }
        
        get <- function() x 

        setINverse <- function(inVerse) 
         {
           inVse1 <<- inVerse
           return(inVse1)
          }
        getINverse <- function() inVse1 
        list(set = set, get = get,
             setINverse= setINverse,
             getINverse = getINverse )


}


##cacheSolve tries to find the the inverse, if its written in first function otherwise it calculates inverse itself and dispays the solution


cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
         inv <- x$getINverse ()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setINverse(inv)
        inv
  
}
