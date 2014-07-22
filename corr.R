corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
		directory <- paste(directory, "/" , sep="")
			temp <- c()
	
		for(num in 1:332)
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

			nobs <- complete(directory , num )$nobs

			if(nobs > threshold)
			{
					file_handle <- file(filename , "rt")
					data_t <- read.csv(file_handle)
					x <- data_t["nitrate"]
					y <- data_t["sulfate"]
	
					x_t <- x[!is.na(x) & !is.na(y)]
					
					y_t <- y[!is.na(x) & !is.na(y)]

					
					temp <- c(temp , cor(x_t,y_t))
					close(file_handle)
#					print (x)
#					print (y)
					}

		}
#			print (temp)
			temp
	}