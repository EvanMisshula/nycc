#+name: inSchool
#+header: :quality 100
#+BEGIN_SRC R :file inSchool.jpeg :results graphics :export both :session nyc 
  nR <- nrow(df)
  inSchool <- df[,'inSchool']

  inSchool_data <- rep(-1,nR)

  inSchool_data[is.na(inSchool)] <- NA
  inSchool_data[as.numeric(inSchool)==1] <-NA
  inSchool_data[as.numeric(inSchool)==2] <- 1
  inSchool_data[as.numeric(inSchool)==3] <- 2     


  inSchoolf <- factor(inSchool_data,levels = 1:2,
                           labels=c("No","Yes"),ordered=TRUE)


  inSchoolTtl <- table(inSchoolf)
  inSchoolPct <- round(100*prop.table(inSchoolTtl),0)
  #jpeg("resp_marginals/ctOnPol.jpg")

  barplot(inSchoolPct, 
main="Respondent is in school percentage sample",
col = "steel blue")

  #dev.off()
#+end_src
