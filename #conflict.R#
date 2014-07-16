#+name: conflict_with_neighbor
#+header: :quality 100
#+BEGIN_SRC R :file conflict_with_neighbor.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  lastNeighborConflictTime <- df[,'lastNeighborConflictTime']

  lastNeighborConflictTime_data <- rep(-1,nR)

  lastNeighborConflictTime_data[is.na(lastNeighborConflictTime)] <- NA
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==1] <-NA
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==2] <-3
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==3] <- 2
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==4] <- 1

  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==5] <- NA
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==6] <- NA
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==7] <- NA
  lastNeighborConflictTime_data[as.numeric(lastNeighborConflictTime)==8] <- 4

  lastNeighborConflictTimef <- factor(lastNeighborConflictTime_data,levels = 1:4,
                           labels=c("never","last-year",
                               "last-month","last-wk"), ordered=TRUE)


  lastNeighborConflictTimeTtl <- table(lastNeighborConflictTimef)
  lastNeighborConflictTimePct <- round(100*prop.table(lastNeighborConflictTimeTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(lastNeighborConflictTimePct, main="Percent had a neighbor conflict in the last 12 months",col = "steel blue")

  #dev.off()
#+end_src
