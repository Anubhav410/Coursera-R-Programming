pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

		##initializing the mean, which is not really required
		directory <- paste(directory, "/" , sep="")
		
		ans <- 0
		temp_ <- c()
		
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
		##	print (temp_data)
			temp_data <- temp_data[pollutant]
			temp_ <- c(temp_ , temp_data[!is.na(temp_data)])
			
			
			close(file_handle)

			
		}
#		print(temp_)
		mean_ <- mean(temp_)
		print(mean_)
		mean_
}