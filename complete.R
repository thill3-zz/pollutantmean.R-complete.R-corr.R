#Problem 2

# Write a function that reads a directory full of files and reports the 
# number of completely observed cases in each data file. The function 
# should return a data frame where the first column is the name of the 
# file and the second column is the number of complete cases.
#e.g. complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463

complete <- function(directory, id) {
        values <- matrix(, nrow = 0, ncol = 2)
        #iterate through the data files
        for (file in id) {
                #initialize the number of observations
                nobs = 0
                #pad with zeroes if necessary
                zeroes_file <- toString(leading_zeroes(file))
                #open up the file
                path <-
                        paste(
                                "G:/My Drive/Coursera Data Science Specialization/",
                                directory,
                                "/",
                                zeroes_file,
                                ".csv",
                                sep = ""
                        )
                data <-
                        read.csv(path) #header = TRUE and sep = "," are defaulted
                #iterate through the records
                for (record in 1:dim(data)[1]) {
                        #if everything is not NA in this record
                        if (all(!is.na(data[record, ]))) {
                                #increment the counter by 1
                                nobs = nobs + 1
                        }
                        #and then move on
                }
                values <- rbind(values, c(file, nobs))
        }
        final_values <-
                data.frame("id" = values[, 1], "nobs" = values[, 2])
        final_values
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

complete("specdata", c(2, 4, 8, 10, 12))
#    id nobs
# 1  2 1041
# 2  4  474
# 3  8  192
# 4 10  148
# 5 12   96
complete("specdata", 30:25)
#   id  nobs
# 1 30  932
# 2 29  711
# 3 28  475
# 4 27  338
# 5 26  586
# 6 25  463
complete("specdata", 3)
#    id nobs
# 1  3  243
complete("specdata", 1)
#    id nobs
# 1  1  117
complete("specdata", 1:10)
