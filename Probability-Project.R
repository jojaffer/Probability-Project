library(readxl)
Online_Retail <- read_excel("~/Documents/Online Retail.xlsx", col_names = FALSE)
retailtimeseries <- ts(Online_Retail)
retailtimeseries
plot.ts(retailtimeseries)
fre1 <- table(Online_Retail)
fre1 <- fre1 / 46540
fre1.data <- data.frame(fre1)
colnames(fre1.data) <- c("States","frequency")
library(ggplot2)
plot1 <- ggplot(data = fre1.data, aes(x= States, y = frequency, group = 1)) + geom_line()
library(markovchain)
Online_Retail_tpm = createSequenceMatrix(Online_Retail, toRowProbs = TRUE)
library(expm)
x2 <- Online_Retail_tpm%^%50
stationary <- x2[1,]
stationary.data <- data.frame(stationary)
stationary.data <- stationary.data[order(as.numeric(rownames(stationary.data))),,drop = FALSE]
stationary.data$state <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
plot2 <- ggplot(data = stationary.data, aes(x= state, y = stationary, group = 1)) + geom_line()
library(gridExtra)
grid.arrange(plot1,plot2)
