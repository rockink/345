sales = read.csv("data.csv")


cor(sales["EU_Sales"], sales["Other_Sales"])
cor(sales["EU_Sales"], sales["NA_Sales"])



plot(sales[,7:11])
cor(sales[,7:11])


#import data 
# descriptive stats
# corelation
#prob modelling