sales = read.csv("data.csv")
attach(data)
genre = sales[,1]

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
  for(i in 1:length(allYears)){
    #for(i in 1:3){
      gameListforThatYear = sales[sales$Year == allYears[i], ]
      # hey but we just want consoles.. 
      gameConsolesForThatYear = table(gameListforThatYear[,"Platform"])
      gameListforThatYearInTable = table(gameConsolesForThatYear)
      #print(gameConsolesForThatYear)
      
      # add them all
      print(allYears[i]); 
      print(gameConsolesForThatYear)
      consoleList[i] = as.vector(gameConsolesForThatYear)
  }
  consoleList
}

consoleList = generateAllConsole(allYears = allYears)
consoleList


#show psp in graph
barplot(table(Year[Platform == "PSP"]), col = rainbow(20))
barplot(table(Year[Platform == "XB"]), col = rainbow(20))
barplot(table(Year[Platform == "PS4"]), col = rainbow(20))

