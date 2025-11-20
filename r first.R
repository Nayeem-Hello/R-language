1+1
2+3*4
3^2
sqrt(10)
pi
x <- 1 	# Can define variables
y <- 3 	# using "<-" operator to set values
z <- 4
x * y * z
X * Y * Z 	# Variable names are case sensitive
str<- "Nayeem"

str<-"Hello World!" #To find the length of a string use the nchar() function
nchar(str)

a <- 80
b <- 50

if (a > b){
  print("a is greater than b")
} else {
  print("a is not greater than b")
}


#IF-ELSE
a <- 200
b <- 33

if (b > a) {
  print("b is greater than a")
} else if (a == b) {
  print("a and b are equal")
} else {
  print("a is greater than b")
}

#IFELSE
score <- 0.3
outcome <- ifelse(score > 0.5, "Passed", "Failed")
print(outcome)

switch(2, "red", "green", "blue")#jehetu 2 disi taii green print korse

switch("shape", 
       "color" = "red", 
       "shape" = "square", 
       "length" = 5)

#while

i <-1
while(i <6) 
{
  print(i) 
  i <- i +1
  }


i <-1
while(i <6) 
{
  print(i) 
  i <- i +1
  if (i == 4) {
    break
  }
  
}


i <-1
while(i <6) 
{
  i <- i +1
  if (i == 4) {
  next
  }
  print(i) 
  
}

for
(x in 1:10) 
{
  print(x)
}


add_numbers <- function(a, b) {
  sum <- a + b
  return(sum)
}
print(add_numbers(4, 5))


##Mathematical operations Using Vector
X <- c(2,4,6,8)
X + 2 
X * 3 
X+ c(3,4,5,6) 

# Sort a Vector
X <- c(20,10,40,15)
sort(X)

X <- c("B", "A", "D")
sort(X)

#Access Vector
X <- c(20,10,40,15)
X[2] 


mymatrix<-matrix(1:20, nrow = 5,ncol = 4)
mymatrix
mymatrix[2,]
mymatrix[,2]
mymatrix[1,3]
mymatrix[1,4]
mymatrix[1,c(2,3)]


#data Frames

patientID <-c(1,2,3,4,5)
age <-c(25,34,28,52,30)
diabetes <- c("type1","Type2","type1","type1","type1")
status<-c("poor","improved","execellent","poor","better")
patientdata <- data.frame(patientID,age,diabetes,status)

patientdata
patientdata[1:3]
patientdata[3]
patientdata[c("diabetes", "status")]


#now add row & Collumn for patient info

item <- list("mango", "apple")
append(item, "orange")

# Add List Items
item <- list("mango", "apple", "Banana")
append(item, "orange", after = 2)


#Remove List Items
item <- list("mango", "apple")
newitem <- item[-2]
newitem
