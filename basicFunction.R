sum <- function(x , y){
		x+y
		
}

above10 <- function(x){	#where x is a list

	#this function will return all the numbers in the list which are bigger than 10

	use <- x > 10 #this is a logical vector, with all the values TRUE where x is greater than 10

	#return the right vector
	x[use]
}

#generic "above" function 
above <- function(x , n) {

	use <- x > n
	
	x[use]
}


#a function that takes in a matrix and retuns a vector of the means of all its columns

columnMean <- function(mat) {
	
	cols = ncol(mat)
	means <- numeric(cols)
	for ( i in 1:cols)
	{
		means[i] <- mean(mat[,i])
	}
	
	means

}