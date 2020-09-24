#Permutations and Combinations
#install.packages("gtools")
library("gtools")

#getwd()
setwd("C:/Users/Administrator/Desktop/R_test_0910/ESE5023")
#combinations() and permutations()
?combinations
combinations(3,2,letters[1:3]) #不考虑顺序
?permutations()
permutations(3,2,letters[1:3]) #考虑顺序

##Use permutations() to show all possible permutations of 
##picking two balls with replacement/repetition from a bag with a red, blue, green, and black ball
permutations(4,2,c("red","blue","green","black"),repeats.allowed = TRUE) #允许重复
permutations(4,2,c("red","blue","green","black"),repeats.allowed = FALSE) #不允许重复

combinations(4,2,c("red","blue","green","black"),repeats.allowed = TRUE) #允许重复
combinations(4,2,c("red","blue","green","black"),repeats.allowed = FALSE) #不允许重复

# compute the annual mean of CO2 since 1959
Keeling_Data <- read.csv(file = "co2_mm_mlo.csv", header = T)
Keeling_Data$co2
Keeling_Data$year

max(Keeling_Data$year) #2020
min(Keeling_Data$year) #1958
temp_year = c(1959:2020)
temp_co2 = c()
for(i in 1959:2020){
  temp_co2 <- c(temp_co2,sum(Keeling_Data$co2[which(Keeling_Data$year == i)]))
}
plot(temp_co2, temp_year,col = "red")

#Compute the annual mean rate of growth of CO2 
temp_co2_rate <- c()
Keeling_Data$co2[which(Keeling_Data$co2 == -99.99)]  <- NA
for(i in 1959:2020){
  dec<-NA
  jan<-NA
  year_ind <- which(Keeling_Data$year == i)
  for(ind in year_ind){
    if(Keeling_Data$month[ind] == "December"){
      dec <-   Keeling_Data$co2[ind]
    }
    if(Keeling_Data$month[ind] == "January"){
      jan <-   Keeling_Data$co2[ind]
    }    
    if(!is.na(dec) && !is.na(jan)){
      temp_co2_rate <- c(temp_co2_rate,dec-jan)
    }
  }
  
}


# Define the function FindTop()
FindTop        <- function(N){
  MoviePage  <- readLines('https://movie.douban.com/top250', encoding='UTF-8')
  Pattarn    <- '<span class=\"title\">&nbsp;/&nbsp;([^<]*)</span>'
  Lines      <- grep(Pattarn, MoviePage, value=TRUE)
  Lines      <- sub(Pattarn,  '\\1', Lines )
  Names      <- sub('^ *', '', Lines)
  Names      <- sub('&#39;', "'", Names)
  print(Names[1:N])
}
# Call the function with arguement 5 and 10
FindTop(5)
FindTop(10)