corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
	
	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0
	
	## Return a numeric vector of correlations
	
	all_completes<-complete(directory,1:332)
	enough_completes<-all_completes[all_completes$nobs>threshold,]
	
	my_vector<-NULL
	
	for(this_id in enough_completes$id){
		input_filename<-paste(directory,"/",formatC(this_id, width=3, flag="0"),".csv", sep="")
		input_dataframe<-read.table(input_filename,header=T,sep=",",quote="\"")
		my_vector<-c(my_vector,cor(x=input_dataframe$nitrate,y=input_dataframe$sulfate,use="complete.obs"))
	}
	
	return(my_vector)
}
