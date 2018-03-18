library(lubridate)

# Load data
data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")

#check class
sapply(data,class)

## Format date
data$Date <- dmy(data$Date, tz="Europe/Berlin")
data$Time <- hms(data$Time)

## Only select complete cases in relevant timeframe (we assume it is Y/M/D as provided on Coursera, not clear)
#Edit: now we know this is the right method
select.data <- subset(data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
select.data <- select.data[complete.cases(select.data),]

# combine time with date
class(select.data$Time) #check
select.data$timeseries <- select.data$Date + select.data$Time
class(select.data$timeseries)

## Plot 1
hist(select.data$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kW)", col="red")

## Plot 2
plot(select.data$Global_active_power ~ select.data$timeseries, type="l", ylab="Global Active Power (kW)", xlab="")

## Plot 3
with(select.data, {
            plot(Sub_metering_1 ~ timeseries, type="l",ylab="Global Activ Power (kW)", xlab="")
            lines(Sub_metering_2 ~ timeseries,col='Red')
            lines(Sub_metering_3 ~ timeseries,col='Blue')
          })

legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), c("Sub metering 1", "Sub metering 2", "Sub metering 3"))

## Plot 4

#create matrix for plots
par(mfrow=c(2,2))

with(select.data, {
  plot(Global_active_power ~ timeseries, type="l", ylab="Global Active Power (kW)", xlab="")
  
  plot(Voltage ~ timeseries, type="l", ylab="Volts (V)", xlab="")
  
  plot(Sub_metering_1 ~ timeseries, type="l", ylab="Global Activ Power (kW)", xlab="")
  lines(Sub_metering_2 ~ timeseries, col='Red')
  lines(Sub_metering_3 ~ timeseries, col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", legend=c("Sub metering 1", "Sub metering 2", "Sub metering 3"))
  
  plot(Global_reactive_power ~ timeseries, type="l", ylab="Global Reactive Power (kW)",xlab="")
})


