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

## create 2 row 2 column plot frame
par(mfrow = c(2,2))

## Let's add the plots we need
with(data, plot(data$Date.Time, data$Global_active_power, ylab = "Global Active Power", xlab = "", type = "l"))

with(data, plot(data$Date.Time, data$Voltage, ylab = "Voltage", xlab = "datetime", type = "l"))

with(data, plot(data$Date.Time, data$Sub_metering_1, xlab="", ylab="Energy sub metering", type = "l", ylim = c(0,38)))
lines(data$Date.Time, data$Sub_metering_2, col = "2")
lines(data$Date.Time, data$Sub_metering_3, col = "4")
legend("topright", col = c("black", "red", "blue"), lty = "solid", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
with(data, plot(data$Date.Time, data$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "l"))

## copy it to plot4.png file
dev.copy(png, file = "plot4.png")
dev.off()
