###
# MajaIV RFID Data Cleaner
###






#Input File 
	#This takes in an excel (xlsx) data set from ONE MajaIV, Version=4.2.0.0 file. Example:
#UTCTime_Creation	Company	Program_Version	UTCTime	ReaderID	Address	UID	ScanCount	Type
#9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:30:08.981	49702	1	15 C6 4F 05 09 00 12 E0	1	177
#9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:33:21.339	49702	1	11 B8 66 01 0B 00 12 E0	1	177
#9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:34:50.449	9519	5	15 C6 4F 05 09 00 12 E0	1	177
#9/27/2016 3:30:10 PM	MICROSENSYS	MajaIV, Version=4.2.0.0, Culture=neutral-4.2.0.0	09/27/2016 15:34:50.681	9519	5	15 C6 4F 05 09 00 12 E0	1	177

#Number of Readers and structure of return flights 

#so an exiting flight would be 4-6, 7-2, 8-5, or 1-3
#return flights would be 6-4, 2-7, 5-8, and 3-1.

#!/usr/bin/env Rscript
#Load Dependancies--------------
	#Note, you must install these locally (e.g. install.packages(c(gdata,dataframes2xls )))
require("gdata")
require("dataframes2xls")


#Read in Dataframe --------------------------
args <- commandArgs(trailingOnly=TRUE) #'example.xlsx'


if(file.exists(args[1])){
	raw.data <- read.xls(args[1]) 
}else{
	print('input file does not exist')
}


#convert columns to time, date, etc ---------------
raw.data[ ,1] <- as.POSIXct(raw.data[ ,1], format="%m/%d/%Y %I:%M:%S %p")  ## Convert to usable date and time of creation (in the PM format)
options(digits.secs=3); raw.data[ ,4] = as.POSIXct(raw.data[ ,4], format="%m/%d/%Y %H:%M:%OS") ## convert reading time to usable in R with milliseconds (and printed milliseconds)
raw.data$UTCTime_Round <- round(raw.data$UTCTime, units = "secs") ## round the milliseconds proper
raw.data <- raw.data[order(raw.data[,4 ]),] ## order by date and time
raw.data <- raw.data[order(raw.data$UID),] ## oreder by ID
raw.data$Date <- lapply(strsplit(as.character(raw.data$UTCTime), " "), "[", 1) ## Get a column with just dates
bees <- as.character(unique(raw.data$UID))


#Output some data checks --------------------------------
print(paste("there are ", length(bees), "samples in your raw dataset", sep=" "))
print(bees)


RF <- raw.data ## I just didn't want to change it in the code 

mung.df <- c()
for(bee in bees){ ##Creating a loop for each variable in dataframe "bees"
	#
	test <- RF[which(RF$UID==bee), ]
	Dates <- as.character(unique(test$Date)) ##creating a list of unique dates
	#low number of rows will cause failure
	for(Date in Dates){  ##creating a loop for each variable in dataframe "Dates"
		#
		date.df <- test[which(test$Date==Date), ]
		ad <- date.df$Address;ad=c(ad, 0) ##creating directionality -- new list of all the column Address plus a zero at the end
		ad1 <- date.df$Address; ad1=c(0, ad1) ##creating directionality -- new list of all the column Address plus a zero at the beginning
		ad <- paste(ad1, ad, sep="-") ##pasting the two lists together
		
		ax <- date.df$UTCTime; ax=c(ax,ax[length(ax)]) ##listing all the times plus an entery that equals the last time
		ax1 <- date.df$UTCTime; ax1=c(ax1[1], ax1) ##listing all the times plus an entery that equals the fist time
		ax <- as.numeric(difftime(ax, ax1, unit="sec")) ##subtracting the two lists and getting output as numeric in seconds
		
		ax <- ax[-1] #removing last row
		ad <- ad[-1] #removing last row
		date.df$timediff <- ax ##adding the time difference column to dataframe
		date.df$dir <- ad ##adding the directionality column to dataframe
		mung.df <- rbind(date.df,mung.df) ##merging the to dateframes without repeats
		print(Date)

	}
	print(bee)
}

mung.df <- mung.df[c("UTCTime_Round", "UID", "timediff", "dir")]


write.xls(mung.df, file = paste(args[2], ".xlsx",sep=""))










##FInal output file = mung.df



