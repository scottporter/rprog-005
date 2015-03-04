complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	#directory<-"//psf/Host/Users/porters/Documents/Experimentation/Coursera/rprog-005/Prog1/rprog-data-specdata/specdata"
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	#id<-c(1,5)
	
	
	
	my_dataframe<-NULL
	
	for(this_id in id){
		input_filename<-paste(directory,"/",formatC(this_id, width=3, flag="0"),".csv", sep="")
		input_dataframe<-read.table(input_filename,header=T,sep=",",quote="\"")
		num_obs<-sum(complete.cases(input_dataframe))
		my_dataframe<-rbind(my_dataframe,as.data.frame(cbind(this_id,num_obs)))
	}
	names(my_dataframe)<-c("id","nobs")
	
	return(my_dataframe)
}
