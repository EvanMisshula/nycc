*** code
#+name: report_crime
#+header: :quality 100
#+BEGIN_SRC R :file report_crime.jpeg :results graphics :export both :session nyc 

#Teachers
reportPolice <- df[,'reportPolice']
ctOnTeachers=rep(-1,nR)


  ctReportPolice[is.na(reportPolice)] <- NA
  ctReportPolice[as.numeric(reportPolice)== 1] <- 5
  ctReportPolice[as.numeric(reportPolice)== 2] <- 4            
  ctReportPolice[as.numeric(reportPolice)== 3] <- 3
  ctReportPolice[as.numeric(reportPolice)== 4] <- 1
  ctReportPolice[as.numeric(reportPolice)== 5] <- 2

  ctReportPolice <- factor(ctReportPolice,levels = 1:5,
                       labels = c("Yes-d",
                           "Yes-p",
                           "Unsure",
                           "No-p",
                           "No-d")
                       ,ordered=TRUE)

#  jpeg("resp_marginals/ctReportPolice.jpg")
  pctcounts <- round(100*table(ctReportPolice),0)
  barplot(tcounts, main="Percentage who would report a crime",col= "steel blue")

#  dev.off()
#+END_SRC
*** graph
#+RESULTS: teacher_trust
