#+name: helped_neighbor
#+header: :quality 100
#+BEGIN_SRC R :file helped_neighbor.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  HelpedNeighbor <- df[,'HelpedNeighbor']

  HelpedNeighbor_data <- rep(-1,nR)

  HelpedNeighbor_data[is.na(HelpedNeighbor)] <- NA
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==1] <-NA
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==2] <-2
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==3] <- 3
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==4] <- 4

  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==5] <- NA
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==6] <- NA
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==7] <- NA
  HelpedNeighbor_data[as.numeric(HelpedNeighbor)==8] <- 1

    HelpedNeighborf <- factor(HelpedNeighbor_data,levels = 1:4,
                           labels=c("last-wk","last-month",
                               "last-year","never"), ordered=TRUE)



  HelpedNeighborTtl <- table(HelpedNeighborf)
  HelpedNeighborPct <- round(100*prop.table(HelpedNeighborTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(HelpedNeighborPct, main="Percent helped a neighbor in the last 12 months",col = "steel blue")

  #dev.off()
#+end_src


