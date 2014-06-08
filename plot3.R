plot3 <- function(){
        file <- "household_power_consumption.txt"
        #Setting the vector for colClasses 
        #to read only 1st, 2nd and three sub_meter columns
        cols<- c(rep("character",2),rep("NULL",4),rep("numeric",3))
        #Creates data.frame of with date, time and sub_metercolumns
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
        
        #Extracting Sub_metering only for date range
        #and plotting vs x=time
        x<- dates[dates>=s_posixlt&dates<e_posixlt]
        df<- data.frame(x=x,df[dates>=s_posixlt&dates<e_posixlt,])
        
        png("plot3.png", width = 480, height = 480)
        with(df, plot(x,Sub_metering_1,ylab="Energy sub metering",
                      xlab="", type="n") )
        with(subset(df),lines(x,Sub_metering_1,ylab="Energy sub metering",
                              xlab="", col="black") )
        with(subset(df),lines(x,Sub_metering_2,ylab="Energy sub metering",
                              xlab="", col="red") )
        with(subset(df),lines(x,Sub_metering_3,ylab="Energy sub metering",
                              xlab="", col="blue") )
        legend("topright", inset=0,lty=1, col=c("black","red","blue"),
               legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
        
        dev.off()
}