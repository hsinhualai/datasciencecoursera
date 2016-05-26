## set the work directory in which contains the household_power_consumption.txt 
setwd( "/Users/hsinhua/datasciencecoursera/ExData_Plotting1")

## Extract the names first
data <- read.table("household_power_consumption.txt", header = T, sep = ";", nrows = 1)
name <- names(data)

## Extract the data we need and assign colnames
data <- read.table("household_power_consumption.txt", header = F, sep = ";", skip =66637, nrows = 2880)
names(data) <- name

## Now we change the class of the first column to be Date
timedata <- strptime(paste(data[,1],data[,2]), "%d/%m/%Y %H:%M:%S")

## Reconstruct the data with 8 columns
data <- data.frame("Date/Time" = timedata,  data[,3:9])

## Now we can show the plot we want and save it as .png file
hist(data[,2], col = "Red", main = "Global Active Power", bg = "White", xlab ="Global Active Power (kilowatts)")
dev.copy(png, file = "plot1.png", width = 480, height = 480)
dev.off()