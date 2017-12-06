sales = read.csv("data.csv", na.strings=c("","N/A","tbd"))
genre = sales[,1]
Year[Year > 2017] = NA

#attach the data frame
attach(data)

## Frequency table for # games released (by Year and by Platform)
freqYearPlatform = table(Year,Platform)

## Plot
# Uncomment the next line for creating a pdf figure
pdf("mygraph.pdf")
year = sort(unique(Year))
matplot(year, freqYearPlatform, type="l", col=1:6, lty=1:5,
        xlab="Year Of Release", ylab="# Games")
title("Game Releases Per Platform")	
legend(1980,500,as.character(levels(Platform)),col=1:6,lty=1:5,cex=.5)
# Uncomment the next line for creating a pdf figure
dev.off()



cor(sales["EU_Sales"], sales["Other_Sales"])
cor(sales["EU_Sales"], sales["NA_Sales"])


plot(sales[,7:11])
cor(sales[,7:11])

hist(table(sales["EU_Sales"]))

#import data 
# descriptive stats
# corelation
#prob modelling




#this is about doing the questions

# lets do first question
colnames = colnames(sales)


consoles = sales[, "Platform"]
#hist(consoles)
#categorizing the data in form of table, for us to then make a list of how the game has developed 
consoleByName = table(consoles)
consoleNum = as.vector(consoleByName)
consoleName = names(consoleByName)


#consoles by year 


# lets do it for the year by 
years = sales[, "Year"]
gamesMadeByYear = table(years)
plot(gamesMadeByYear)


# create empty object 
# my idea is to see how the consoles number changed
# number of sales of games happened in console per year... 
# this can show how consoles and games are related..
# should be directly proportional 
# so, what do we need? 
# console by year 
# get each year, 
# and in each year, categorize the console 

# first get each year... 
allYears = names(gamesMadeByYear)
allYears


generateAllConsole <- function(allYears){
  consoleList = matrix(nrow = length(allYears), ncol = length(consoleName))
  gameListforThatYear = sales[sales$Year == allYears[0], ]
  
  for(i in 1:length(allYears)){
  #for(i in 1:3){
    gameListforThatYear = sales[sales$Year == allYears[i], ]
    # hey but we just want consoles.. 
    gameConsolesForThatYear = table(gameListforThatYear[,"Platform"])
    gameListforThatYearInTable = table(gameConsolesForThatYear)
    #print(gameConsolesForThatYear)
    
    consoleList[i,] = as.vector(gameConsolesForThatYear)
  }
  consoleList
}

consoleList = generateAllConsole(allYears = allYears)
rownames(consoleList) = as.vector(allYears)
colnames(consoleList) = as.vector(consoleName)

consoleList

barplot(consoleList[,c("DS","PS2")], col=c("darkblue", "red","yellow","green","orange"), legend=rownames(consoleList))
legend("bottom", legend = consoleName, pch=10:19, horiz = TRUE)
#show psp in graph

#just ps2 and ds 
matplot(tdData, type="b", pch = 10:19)
legend("topleft", legend = c("DS","PS2"), pch=10:19)
sum(tdData[,c("DS")]) #2163
sum(tdData[,c("PS2")]) #2161

barplot(table(Year[Platform == "PSP"]), col = rainbow(20))
barplot(table(Year[Platform == "XB"]), col = rainbow(20))
barplot(table(Year[Platform == "PS4"]), col = rainbow(20))


# this calculates the number of consoles active 
# in a year.. this is the answer to question 1.2b. 
numConsoles = function(freqYearPlatform, allYears){
  
  consolesByYear = numeric(0)
  len = length(allYears) - 1
  for(i in 1:len){
    #basic idea is to count if games sold is > 0, 0 meaning its absent
    consolesByYear[toString(allYears[i])] = sum(freqYearPlatform[toString(allYears[i]),] > 0)
  }
  
  consolesByYear
}

consoleByYear = numConsoles(freqYearPlatform, allYears)
consoleByYear
barplot(consoleByYear, col=rainbow(20), ylim = c(0,12))


matplot(consoleByYear, freqYearPlatform, type="l", col=1:6, lty=1:5,
        xlab="Num of active game sales by year", ylab="# Games")


