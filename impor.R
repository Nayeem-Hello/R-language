#install.packages("dplyr")
#Reading Data from a csv file
mydataframe <- read.csv(file, header=logical_value, sep="delimiter")

mydata<- read.csv("C:/Users/User/Downloads/Iris(2)(2).csv",header = TRUE,sep = ",")##mydata kota j lekha eta hocce dataframe
mydata
#selecting the 5th to 10th row of the iris dataset.
mydata[5:10,]

#How to select columns in R
mydata$Species
mydata$Id

#How to select subset data in R

subset(mydata,Species=="Iris-virginica")
subset(mydata,Species=="Iris-setosa")


subset(mydata,PetalLengthCm>=6)
subset(mydata,SepalLengthCm>=6)
subset(mydata,SepalWidthCm>=6)
subset(mydata,PetalWidthCm>=2)

# filter(dataframeName, condition)

library(dplyr)
filter(mydata, SepalLengthCm > 5)
filter(mydata, Species == "Iris-setosa")
filter(mydata, SepalLengthCm > 5 & Species == "Iris-setosa")
filter(mydata, PetalWidthCm < 0.3)

#distinct(dataframeName, col1, col2,.., .keep_all=TRUE)
#distinct() method removes duplicate rows from data frame or based on the specified columns
library(dplyr)

 distinct(mydata, PetalLengthCm, PetalWidthCm, .keep_all = TRUE)

 

 #Descriptive statistics  ## specific collum dekte chaile
 
 vars<-c ("SepalLengthCm", "SepalWidthCm")
 mydata[vars]
 head(mydata[vars],6)  # mane 6 ta row pojonto dekte chaile

 #Annotating datasets 
 mydata$Species <- factor(
   mydata$Species,
   levels = c("Iris-setosa", "Iris-versicolor", "Iris-virginica"),
   labels = c(1, 2, 3)
 )
 str(mydata)
 #standard deviation ber korar formula kunu attributer er jonno
 s<-mydata$SepalLengthCm
 sd(s)
 
 ##for all atrribute standard deviation calculation
 library(dplyr)
 mydata %>% summarise_if(is.numeric, sd)
 
 library(dplyr)
 
 sample_n(mydata,3)
 
 #specific variable er jonno
 mydata2 = select(mydata, SepalLengthCm )
 mydata2
 
 #missing value ase kina ..true mane missing value ase ..just dataset ee true dekhabe jodi missing thake kunu value
 is.na(mydata)
 
 colSums(is.na(mydata)) #mane kotota missing value ase 1 ta collum ee hisab kore
 
 which(is.na(mydata$Species))
 which(is.na(mydata$SepalWidthCm)) # koto number row te missing value ase ta dekhay
 
 remove<- na.omit(mydata)
 
 View(mydata) 