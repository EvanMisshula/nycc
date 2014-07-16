cIdx <- 91+2*(0:7)

for(i in cIdx) {
    print(table(as.numeric(df[,i])))
      }

RcommP_data <- rep(-1,nrow(df))
RcommP1 <- as.numeric(df[,'RcommP1L12mo'])==3
RcommP2 <- as.numeric(df[,'RcommP2L12mo'])==3
RcommP3 <- as.numeric(df[,'RcommP3L12mo'])==3
RcommP4 <- as.numeric(df[,'RcommP4L12mo'])==3
RcommP5 <- as.numeric(df[,'RcommP5L12mo'])==3
RcommP6 <- as.numeric(df[,'RcommP6L12mo'])==3
RcommP7 <- as.numeric(df[,'RcommP7L12mo'])==3
RcommP8 <- as.numeric(df[,'RcommP8L12mo'])==3


RcommP_data[RcommP1 | RcommP2 | RcommP3 | RcommP4 | RcommP5 | RcommP6 | RcommP7 | RcommP8] <- 2
RcommP_data[!RcommP1 & !RcommP2 & !RcommP3 & !RcommP4 & !RcommP5 & !RcommP6 & !RcommP7 & !RcommP8] <- 1

AnyRcommP <- factor(RcommP_data,levels = 1:2,labels = c("No","Yes"),ordered = TRUE)

tAnyRcommP <- round(100*prop.table(table(AnyRcommP)),0)
barplot(tAnyRcommP,main = "Percentage of Respondents that speak regularly to a program employee", col="steel blue")
