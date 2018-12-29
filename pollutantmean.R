#problem 1
pollutantmean <- function(directory, pollutant, id = 1:332) {
        #get all of the data for that pollutant from all of the files
        #find the mean while ignoring all of the NAs
        #--#create a master list for the values
        master_values_vector <- vector()
        #--#for each value in 'directory'
        for (file in id){
                #pad with zeroes if necessary
                file <- toString(leading_zeroes(file))
                #--#use paste to create the file path+name
                path <- paste("G:/My Drive/Coursera Data Science Specialization/", directory, "/", file, ".csv", sep = "")
                #--#source(G:/My Drive/Coursera Data Science Specialization/specdata/...)
                data <- read.csv(path) #header = TRUE and sep = "," are defaulted
                #--#create a new vector with the values for that pollutant
                index_no <- which(colnames(data)==pollutant)
                values <- data[,index_no]
                #--#find the NAs in that vector
                nas_in_vector <- is.na(values)
                #--#get all of the real values by avoiding the NAs
                used_values <- values[!nas_in_vector]
                #--#add the values onto the master list
                master_values_vector <- c(master_values_vector, used_values)
        }
        #--#find the mean of the master list
        average <- mean(master_values_vector)
        #--#return that mean           
        return (average)
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
pollutantmean("specdata", "sulfate", 1:10)  #[1] 4.064128
pollutantmean("specdata", "nitrate", 70:72) #[1] 1.706047
pollutantmean("specdata", "nitrate", 23)    #[1] 1.280833