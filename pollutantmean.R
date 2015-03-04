pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	#directory<-"//psf/Host/Users/porters/Documents/Experimentation/Coursera/rprog-005/Prog1/rprog-data-specdata/specdata"
	
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".
	#pollutant<-"nitrate"
	
	
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
	
	#id<-c(1,5)
	
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	
	input_filenames<-paste(directory,"/",formatC(id, width=3, flag="0"),".csv", sep="")
	my_vector<-NULL
	
	for(input_filename in input_filenames){
		input_dataframe<-read.table(input_filename,header=T,sep=",",quote="\"")
		#my_vector<-c(my_vector,input_dataframe[,pollutant])
		my_vector<-c(my_vector,input_dataframe[!is.na(input_dataframe[,pollutant]),pollutant])
	}
	
	#mean(my_vector,na.rm=TRUE)
	mean(my_vector)
}
