# Part 3
#
# Write a function that takes a directory of data files and a threshold for
# complete cases and calculates the correlation between sulfate and nitrate for
# monitor locations where the number of completely observed cases (on all
# variables) is greater than the threshold. The function should return a vector
# of correlations for the monitors that meet the threshold requirement. If no
# monitors meet the threshold requirement, then the function should return a
# numeric vector of length 0. A prototype of this function follows
#
# For this function you will need to use the 'cor' function in R which calculates
# the correlation between two vectors. Please read the help page for this
# function via '?cor' and make sure that you know how to use it.
#
# Please save your code to a file named
# corr.R. To run the submit script for this part, make sure your working
# directory has the file corr.R in it.

complete_directory <- complete("specdata", 1:332)

corr <- function(directory, threshold = 0) {
        #do this at the start because it takes so long.
        #complete_directory <- complete(directory, 1:332)
        correlation_vector <- vector()
        for (file in 1:332) {
                #every recording file
                zeroes_file <- toString(leading_zeroes(file))
                path <-
                        paste(
                                "G:/My Drive/Coursera Data Science Specialization/",
                                directory,
                                "/",
                                zeroes_file,
                                ".csv",
                                sep = ""
                        )
                data <- #This will be one file
                        read.csv(path) #header = TRUE and sep = "," are defaulted
                if (complete_directory[file, 2] > threshold) {
                        #if it has enough complete records
                        combined <- na.omit(data[,2:3])
                        sulfate <- combined[,1]
                        nitrate <- combined[,2]
                        corr <- cor(sulfate, nitrate) #the correlation
                        correlation_vector <- c(correlation_vector, corr)
                }
                #if (length(correlation_vector) %% 5 == 0) {
                #        print(correlation_vector)
                #}
        }
        if (length(correlation_vector) == 0) {
                return(vector())
        } else {
                return(correlation_vector)
        }
}

leading_zeroes <- function(n) {
        new_num <- toString(n)
        if(n >= 0 && n <= 99) {
                num_zeroes <- 3 - nchar(n)
                for (i in 1:(num_zeroes)) {
                        new_num <- paste("0", new_num, sep = "")
                }
        }
        new_num
}

cr <- corr("specdata", 150)
cr
head(cr)
# > head(cr)
# [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
summary(cr)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 

cr <- corr("specdata", 400)
head(cr)
# [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860
summary(cr)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313 

cr <- corr("specdata", 5000)
summary(cr)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0       0       0       0       0       0 
length(cr)
# [1] 1

cr <- corr("specdata")
summary(cr)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
length(cr)
# [1] 323
