## makeCacheMatrix Function Description : 
# This function creates a list which holds the address of # setter and getter functions to a) get the value of the # matrix b) set the value of the matrix c) get the inverse# of the matrix and d) set the value of the matrix. 

## Global environment with '<<':
# This function uses the special assignment operator('<<') to# create these functions and the variables in a global # environment to cache the value of the matrix and also# the inverse of the matrix

#argument datum of the makeCacheMatrix is the input matrix#inversedatum is initiliazed to NULL, this variable holds#the inverse data of the given matrix


makeCacheMatrix<-function(datum=matrix()){    
        inversedatum<-NULL  
  #setter function to set the input data for the matrix  
        set<-function(moddatum){      
  #new value of moddatum is assigned to the global environment variable dataum   
        datum<<-moddatum       
  #since it is a new data, the inverse of any previous data to be cleared  
        inversedatum<<-NULL        
  }
  #getter function to get the input matrix data  
        get<-function()datum   
  #setter function to set the inverse of the input matrix and this get saved in the global environment 
        setinverse<-function(inverse)inversedatum<<-inverse    
  #we want to retrieve the inverse matrix of the given data with this function  
        getinverse<-function()inversedatum    
  #Here, basically we are getting the pointers to the  # the functions which are defined in the global environment 
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)}

  #This function is the one which does the inversion for any# new matrix values or returns the cached inverted matrix 
  #for any previous data
cacheSolve<-function(datum,...){ 
  #Let's try to get the inverse of the input data  
        inversedatum<-datum$getinverse()    
  #Check what we have got is NULL (to know it is new or old)
        if(!is.null(inversedatum))  {  
  #we have the same data defined and so no computation    
        message("getting cached data")    
        message("The given matrix is")
        print(datum$get()) 
        message("Inverse of the given matrix is")
  #only return the cached data, we saved time here !
        return(inversedatum)  }    
  # we did not have the solved matrix before
        message("The given matrix is") 
        print(data<-datum$get())
  #now we are caculating the inverse of the matrix for new data
        inversedatum<-solve(data,...) 
  #Let's save it for future retrieval, if we don't, we have to keep computing everytime
        datum$setinverse(inversedatum)
        message("Inverse of the given matrix is")
  #Let's print the inverted matrix here for the new data
        inversedatum}
