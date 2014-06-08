plot1 <- function(){
        file <- "household_power_consumption.txt"
        #Setting the vector for colClasses 
        #to read only 1st and 3rd column
        cols<- c(rep("character",1),rep("NULL",1),
                 rep("numeric",1),rep("NULL",6))
        #Creates data.frame of 1st and 3rd columns
        df<- read.table(file, sep=";", header=TRUE, 
                        na.strings="?", colClasses=cols)
        
        #Setting start date and end date in POSIX
        start_Date<- as.Date("2007-02-01")
        end_Date<- as.Date("2007-02-03")
        s_posixlt<-as.POSIXlt(start_Date)
        e_posixlt<-as.POSIXlt(end_Date)
        #Converting date column to vector of POSIXlt date
        dates<-strptime(df[,1],"%d/%m/%Y", tz="UTC")
        
        #Extracting Global Active Power only for date range
        #and preparing histogram
        x<- df[dates>=s_posixlt&dates<e_posixlt,"Global_active_power"]
        hist(x, col="red", xlab="Global Active Power (kilowatts)",
             main="Global Active Power")
        #Copying to PNG file from screen
        dev.copy(png, file="plot1.png")
        dev.off()
}