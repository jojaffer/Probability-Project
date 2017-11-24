##First 50,000 samples - filtering out values that rund to 0 or over 20
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

##Third 50,000 samples - filtering out values that rund to 0 or over 20
Online_Retail_2 <- read_excel("~/Documents/Online Retail 2.xlsx", col_names = FALSE)
Online_Retail_2$X0 <- NULL
Online_Retail_2 <- Online_Retail_2[complete.cases(Online_Retail_2),]
retailtimeseries3 <- ts(Online_Retail_2)
retailtimeseries3
plot.ts(retailtimeseries3)
fre3 <- table(Online_Retail_2)
fre3 <- fre3 / 45713
fre3.data <- data.frame(fre3)
colnames(fre3.data) <- c("States","frequency")
library(ggplot2)
plot5 <- ggplot(data = fre3.data, aes(x= States, y = frequency, group = 1)) + geom_line()
library(markovchain)
Online_Retail_2_tpm = createSequenceMatrix(Online_Retail_2, toRowProbs = TRUE)
library(expm)
z2 <- Online_Retail_2_tpm%^%50
stationary3 <- z2[1,]
stationary3.data <- data.frame(stationary3)
stationary3.data <- stationary3.data[order(as.numeric(rownames(stationary3.data))),,drop = FALSE]
stationary3.data$state <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
plot6 <- ggplot(data = stationary3.data, aes(x= state, y = stationary3, group = 1)) + geom_line()
library(gridExtra)
grid.arrange(plot5,plot6)

##Simulation Time Series
simulation <- runif(500,1,20)
simulation <- round(simulation, digits = 0)
simulationtimeseries <- ts(simulation)
plot.ts(simulationtimeseries)
simfre <- table(simulation)
simfre <- simfre / 500
simfre
simfre.data <- data.frame(simfre)
library(ggplot2)
plot7 <- ggplot(data = simfre.data, aes(x = simulation, y = Freq, group = 1)) + geom_line()
grid.arrange(plot1,plot7)