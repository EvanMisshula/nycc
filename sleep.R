#+name: sleepTime
#+header: :quality 100
#+BEGIN_SRC R :file sleepTime.jpeg :results graphics :export both :session nyc 
sleepTimeC <- gsub("M ","M",gsub("  ","",levels(df$sleepTime)))


sleepData=rep(-1,nR)
sleepData[sleepTimeC=="11 AM"] <- 8
sleepData[sleepTimeC=="12 PM (Noon)"] <- 8
sleepData[sleepTimeC=="1 PM"] <- 9
sleepData[sleepTimeC=="2 PM"] <- 9            
sleepData[sleepTimeC=="3 PM"] <- 10
sleepData[sleepTimeC=="4 PM"] <- 10
sleepData[sleepTimeC=="5 PM"] <- 11           
sleepData[sleepTimeC=="6 PM"] <- 11
sleepData[sleepTimeC=="7 PM"] <- 12
sleepData[sleepTimeC=="8 PM"] <- 12
sleepData[sleepTimeC=="9 PM"] <- 1
sleepData[sleepTimeC=="10 PM"] <- 1
sleepData[sleepTimeC=="11 PM"] <- 2
sleepData[sleepTimeC=="12 AM (Midnight)"] <- 2
sleepData[sleepTimeC=="1 AM"] <- 3
sleepData[sleepTimeC=="2 AM"] <- 3
sleepData[sleepTimeC=="3 AM"] <- 4            
sleepData[sleepTimeC=="4 AM"] <- 4
sleepData[sleepTimeC=="5 AM"] <- 5
sleepData[sleepTimeC=="6 AM"] <- 5
sleepData[sleepTimeC=="7 AM"] <- 6
sleepData[sleepTimeC=="8 AM"] <- 6
sleepData[sleepTimeC=="9 AM"] <- 7
sleepData[sleepTimeC=="10 AM"] <- 7


sleepLabels <- c("e-night","m-night","l-night",
                 "e-morn","m-morn","l-morn",
                 "e-after","m-after","l-after",
                 "e-eve","m-eve","l-eve"
                 )
                 
sleep <- factor(sleepData,levels = 1:12,labels = sleepLabels,ordered = TRUE)

counts <- round(100*prop.table(table(sleep)),0)
barplot(counts, main="Sleep Time")

