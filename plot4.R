plot4 <- function(){
        file <- "household_power_consumption.txt"
        #Setting the vector for colClasses 
        #to read only 1st character column
        cols<- c(rep("character",2),rep("numeric",3),
                 rep("NULL",1),rep("numeric",3))
        #Creates data.frame of selected columns
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
        
        #Extracting selected fields only for date range
        #and plotting vs x=time
        x<- dates[dates>=s_posixlt&dates<e_posixlt]
        df<- data.frame(x=x,df[dates>=s_posixlt&dates<e_posixlt,])
        #Plotting in PNG device in 2 by 2
        png("plot4.png", width = 480, height = 480)
        par(mfrow=c(2,2), mar=c(4,4,2,1))
        #1st row, 1st column plot
        plot(df$x, df$Global_active_power,type="l",
             ylab="Global Active Power",xlab="")
        #1st row, 2nd column plot
        plot(df$x, df$Voltage,type="l",
             ylab="Voltage",xlab="datetime")
        #2nd row, 1st column plot
        with(df, plot(x,Sub_metering_1,ylab="Energy sub metering",
                      xlab="", type="n") )
        with(subset(df),lines(x,Sub_metering_1,ylab="Energy sub metering",
                              xlab="", col="black") )
        with(subset(df),lines(x,Sub_metering_2,ylab="Energy sub metering",
                              xlab="", col="red") )
        with(subset(df),lines(x,Sub_metering_3,ylab="Energy sub metering",
                              xlab="", col="blue") )
        legend("topright", inset=0,bty="n", lty=1, col=c("black","red","blue"),
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        #2nd row, 2nd column plot
        plot(df$x, df$Global_reactive_power,type="l",
             ylab="Global_reactive_power",xlab="datetime")
        
        dev.off()
}