complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
		
		directory <- paste(directory, "/" , sep="")

		total <- c()
		for( num in id)
		{
			if(num < 10)
			{
				filename <- paste(c("00") , num , sep="")
				filename <- paste(filename , ".csv" , sep = "")
			}
			else if(num > 9 & num < 100)
			{
				filename <- paste("0" , num , sep="")
				filename <- paste(filename , ".csv" , sep = "")
			}
			else
			{
				filename <- paste(num , ".csv" , sep = "")
			}
			
			filename <- paste(directory , filename , sep = "")
			file_handle <- file(filename , "rt")
			temp_data <- read.csv(file_handle);
			temp_nitrate <- temp_data["nitrate"]
			temp_sulfate <- temp_data["sulfate"]

			temp_multiplied = temp_nitrate *  temp_sulfate
			temp_logical = !is.na(temp_multiplied)
			
			total <- c(total , sum(temp_logical))

			
#			print(total)
#			temp_ <- c(temp_ , temp_data[!is.na(temp_data)])
				
			
			close(file_handle)

			
		}
#		print (total)
		ans <- data.frame(id, total )
		colNames <- c("id" , "nobs")
		colnames(ans) <- colNames
		
		ans
	
}