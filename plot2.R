plot2 <- function(){
        file <- "household_power_consumption.txt"
        #Setting the vector for colClasses 
        #to read only 1st,2nd and 3rd column
        cols<- c(rep("character",2),rep("numeric",1),rep("NULL",6))
        #Creates data.frame of 1st,2nd and 3rd column
        df<- read.table(file, sep=";", header=TRUE, 
                        na.strings="?", colClasses=cols)
        
        #Setting start date and end date in POSIX
        start_Date<- as.Date("2007-02-01")
        end_Date<- as.Date("2007-02-03")
        s_posixlt<-as.POSIXlt(start_Date)
        e_posixlt<-as.POSIXlt(end_Date)
        
        #Converting date column to vector of POSIXlt date
        dates<-strptime(paste(df[,1],df[,2],sep=" "),
                        "%d/%m/%Y %T", tz="UTC")
        #Reading Global Active Power
        #Extracting Global Active Power only for date range
        #and plotting vs x=time
        y<- df[dates>=s_posixlt&dates<e_posixlt,"Global_active_power"]
        x<- dates[dates>=s_posixlt&dates<e_posixlt]
        plot(x, y,type="l", ylab="Global Active Power (kilowatts)",
             xlab="")
        
        #Copying to PNG file from screen
        dev.copy(png, file="plot2.png")
        dev.off()
}