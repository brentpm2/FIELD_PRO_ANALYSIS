library(readxl)   
weather_response_graph <- function(filename,trialname,x,y,graphtype="p",sheetnumber=3,...){
  #function will read in a xls file that was outputted by MTA for Field Pro and a specific trial name from that data set
  #the user has the option to choose what kind of plot is made
  #If no plot type is selected, a basic scatterplot will be created
  #If no plot type is selected or "p" is selected a regression line will also be output
  #will output a graph of the chosen type with the x variables as the trial treatment number
  #y variable will be yield data in Kg/hectare
  #the plot will be named after the trial name that was given by the user
  
  #Reads in the selected sheet number 
  graphfile<-read_xls(filename, sheet = sheetnumber)
  
  #Will check to see if TRIAL TRT# is in the file given and if it is not will give an error
  if(!"TRIAL TRT#" %in% names(graphfile)){
    stop("Excel File is missing TRIAL TRT# column")
  }
  #Will check to see if DATA is in the file given and if it is not will give an error
  if(!"DATA" %in% names(graphfile)){
    stop("Excel File is missing DATA column")
  }
  #If the trial TRIAL TRT# colum is not numeric this will stop the function
  if(!is.numeric(graphfile$"TRIAL TRT#")){
    stop("The trial treatment number column needs to be numeric")
  }
  #If the DATA is not numeric this will stop the function
  if(!is.numeric(graphfile$"DATA")){
    stop("The Data column needs to be numeric")
  }
 
  #creates the x variable and fills it with the trial treatment number for the specific trial that the user inputed
  xvar<-graphfile$"TRIAL TRT#"[graphfile$"TRIAL #"==trialname]
  #creates the y variable and fills it with the yield data for the specific trial that the user inputed
  yvar<-graphfile$"DATA"[graphfile$"TRIAL #"== trialname]
  
  #creates a scatterplot with a regression line when graphtype is equal to "p"
  #creates a plot of the selected type without a regression line when graphtype does not equal "p"
  if(graphtype=="p"){
    #plots the x variable by the y variable and adds labels to both x and y axis as well as the plot label
    plot(xvar,yvar,xlab="Trial treatment number",ylab="Yield kg/hectare",main=trialname,type=graphtype,... )
    #adds a fitline for the data
    abline(fit<-lm(yvar~xvar))
    legend("topright", bty="n", legend=paste("Adjusted R^2", format(summary(fit)$adj.r.squared, digits=4)))
  }else{
    if(graphtype=="h"){
      hist(yvar, xlab = "Yield kg/hectare",...)
    }else{
      #plots the x variable by the y variable and adds labels to both x and y axis as well as the plot label
    plot(xvar,yvar,xlab="Trial treatment number",ylab="Yield kg/hectare",main=trialname,type=graphtype,... )
    }
    
  }
graphfile
}



