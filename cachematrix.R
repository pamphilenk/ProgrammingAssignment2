## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## COMMENTS
## Matrices are vectors with a dimension attribute. For an inversible matrix this dimension may be c(x,x). it's squared
# PLEASE SEE COMMENTS FRONT OF EACH LINE for more details.

makeCacheMatrix <- function(x = matrix()) {
        
        #This function creates a special "matrix" object that can cache its inverse.
        
        invm <- NULL                # invm for inverse.///   sets with null
        
        
        set <- function(y){         
                m <<- y              # sets the matrix m
                invm <<- NULL        # init the inverse of m
        }
        
        get <- function() m          # gets value of matrix m
        
        
        setdim <- function(dim) dimm <<- c(x = integer(), x = integer()) #sets definining dimension for m as
                                                                         #... a squared matrix : ncol = nrows
        getdim <- function() dimm                                        #gets the dim
        
        
        setinv <- function(solve) invm <<- solve                         # solve 
        
        getinv <- function() invm                                        # gets the inverse
        
        
        list(set = set, get = get, setdim = setdim, getdim = getdim, setinv = setinv, getinv = getinv )

}


## Write a short comment describing this function
## PLEASE SEE COMMENTS ON FRONT OF EACH LINE

cacheSolve <- function(m, ...) {
        
        ## Return a matrix that is the inverse of 'm'
        #This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
        
        invm <- m$getinv()                      # gets the inverse
        
        if(!is.null(invm)){                     # if already computed then loadcache and 
                
                matinvm <- loadCache(invm)     #gets the loaded value. The available inverse
                
                return(matinvm)                #give the inverse and break
        }                                       
        #ELSE                                  if inverse not already computed then ...
        
        matm <- m$get()                        #gets the matrix
        
        m$setdim(matm)                         # sets the dimension
        
        invm <- solve(matm)                    # calculate the inverse
        
        m$setinv(invm)                          
        
        invm                                    #return the imverse
}
