## Put comments here that give an overall description of what your
## functions do

##This function is used to calculate the inverse of the matrix using the cached data in the 
##environment.First of all, we are making the matrix to be stored inside the environment
## and its inverse calculated if not already present in the cached place.

## Write a short comment describing this function

#this function takes the input of matrix x and im is the inverse value of matrix,
#initially we set the value of y to x in to the environment for later use.
#The get function returns the matrix entered and the setInverse function is used to 
#get the inverse by using the solve function and storing it in im.
#getInverse is used to return the value of im.
#Finally the list is returned for the function makeCacheMatrix() which contains the set,
#get,setInverse and getInverse to be used.


makeCacheMatrix <- function(x = matrix()) {

   im<-NULL
   set<-function(y){
      x<<-y
      im<<-NULL
   }
   get<-function() x
   setInverse<-function(solve) im<<-solve
   getInverse<-function() im
   
   list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
   
}


## Write a short comment describing this function

#cahceSolve is used to retrieve the value of the inverse of the matrix x entered in above
#function and finally if it is already present in cache, it is returned by the x$getInverse()
#If the value is null, we calculate the inverse using solve of x which is stored above in the 
#parent environment and is finally returned as im.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   im<-x$getInverse()
   
   if(!is.null(im)){
      message("getting cached data")
      return(im)
   }
   
   matrixdata<-x$get()
   im<-solve(matrixdata,...)
   x$setInverse(im)
   
   im
}
