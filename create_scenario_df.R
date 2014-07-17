create_scenario_df <- function(df,scenarios)
    {
        ddf <- data.frame(obs=1:nrow(df))

        ## ddf$FFightPartyNotHere <- Fscenarios[['FFightPartyNotHere']]   
        ## ddf$FBrothersAttacker <- Fscenarios[['FBrothersAttacker']]    
        ## ddf$FBumpSwingNotHere <- Fscenarios[['FBumpSwingNotHere']]    
        ## ddf$FFriendAttacked <- Fscenarios[['FFriendAttacked']]      
        ## ddf$FStolenJacketPublic <- Fscenarios[['FStolenJacketPublic']]
        
        ## ddf$FOwedMoneyPlayed <- Fscenarios[['FOwedMoneyPlayed']]     
        ## ddf$FMoneyOwed <- Fscenarios[['FMoneyOwed']]           
        ## ddf$FBetWelch <- Fscenarios[['FBetWelch']]            
        ## ddf$FPublicDis <- Fscenarios[['FPublicDis']]           
        ## ddf$FBodegaCheat <- Fscenarios[['FBodegaCheat']]         

       
        ## ddf$FBasketball <- Fscenarios[['FBasketball']]          
        ## ddf$FJordans <- Fscenarios[['FJordans']]             
        ## ddf$FSocialMedia <- Fscenarios[['FSocialMedia']]         
        ## ddf$FexGirlPlayed <- Fscenarios[['FexGirlPlayed']]        
        ## ddf$FGirlClaim <- Fscenarios[['FGirlClaim']]           

        ## ddf$FGirlsNewG <- Fscenarios[['FGirlsNewG']]           

        ## ddf <- ddf[, 2:17]
        
        ddf$RFightPartyNotHere <- dfnbx$RFightPartyNotHere   
        ddf$RBrothersAttacker <- dfnbx$RBrothersAttacker    
        ddf$RBumpSwingNotHere <- dfnbx$RBumpSwingNotHere    
        ddf$RRriendAttacked <- dfnbx$RFriendAttacked      
        ddf$RStolenJacketPublic <- dfnbx$RStolenJacketPublic  

        ddf$ROwedMoneyPlayed <- dfnbx$ROwedMoneyPlayed     
        ddf$RMoneyOwed <- dfnbx$RMoneyOwed           
        ddf$RBetWelch <- dfnbx$RBetWelch            
        ddf$RPublicDis <- dfnbx$RPublicDis           
        ddf$RBodegaCheat <- dfnbx$RBodegaCheat         

        ddf$RBasketball <- dfnbx$RBasketball          
        ddf$RJordans <- dfnbx$RJordans             
        ddf$RSocialMedia <- dfnbx$RSocialMedia         
        ddf$RexGirlPlayed <- dfnbx$RexGirlPlayed        
        ddf$RGirlClaim <- dfnbx$RGirlClaim           

        ddf$RGirlsNewG <- dfnbx$RGirlsNewG           

        ddf <- ddf[,2:17]

        respWeight <- matrix(-1,nrow=nrow(ddf),ncol=16)
#        FrespWeight <- matrix(-1,nrow=nrow(ddf),ncol=16)

        resp <-matrix(-1,nrow=nrow(ddf),ncol=16)
 #       Fresp <- matrix(-1,nrow=nrow(ddf),ncol=16)
        
        for(j in 1:16) {
            resp[,j] <- as.numeric(ddf[,j])
  #          Fresp[,j] <- as.numeric(ddf[,j+16])
        }

        for(j in 1:16) {
            respWeight[ is.na(resp[,j]) | resp[,j] == 6,j] <- 0
            respWeight[!is.na(resp[,j]) & resp[,j] < 6,j] <- 1

   #         FrespWeight[ is.na(Fresp[,j]) | Fresp[,j] == 6,j] <- 0
   #         FrespWeight[!is.na(Fresp[,j]) & Fresp[,j] < 6,j] <- 1
    }
        
        validRespCount <- apply(respWeight,1,sum)
    #    FvalidRespCount <- apply(FrespWeight,1,sum)

        respVioIndex <- rep(NA,nrow(ddf))
     #   FrespVioIndex <- rep(NA,nrow(ddf))        
        
        respVioIndex[validRespCount!=0] <- apply((resp[validRespCount!=0,]
                         * respWeight[validRespCount!=0,] *
                         matrix(rep(1/validRespCount[validRespCount!=0],16),
                                ncol = 16, byrow = FALSE)),1,sum)

      #  FrespVioIndex[FvalidRespCount!=0] <- apply((Fresp[FvalidRespCount!=0,]
       #                  * FrespWeight[FvalidRespCount!=0,] *
        #                 matrix(rep(1/FvalidRespCount[FvalidRespCount!=0],16),
         #                       ncol = 16, byrow = FALSE)),1,sum)


        ddf$validRespCount <- validRespCount
       # ddf$FvalidRespCount <- FvalidRespCount
        ddf$respVioIndex <- respVioIndex
      #  ddf$FrespVioIndex <- FrespVioIndex
       # ddf$vioIndex <- .5*(respVioIndex+FrespVioIndex)

        vic<- data.frame(obs=1:nrow(ddf))

        vic$EverShotAt <- df$EverShotAt                           
        vic$ShotAtCount <- df$ShotAtCount                          
        vic$EverStabbed <- df$EverStabbed                          
        vic$StabbedCount <- df$StabbedCount                         
        vic$GunsSeenCount <- df$GunsSeenCount                        
        vic$HeardPersonThreatenedWGunHereL12Count <- df$HeardPersonThreatenedWGunHereL12Count
        vic$HeardGunshotsHereL12Count <- df$HeardGunshotsHereL12Count            
        vic$REverSQF <- df$REverSQF

        fit <- aov(respVioIndex ~ EverShotAt,data=vddf)
        summary(fit)
        
        plotmeans(vddf$respVioIndex~vddf$EverShotAt,
                  xlab = "Does the Respondent report having been shot at",
                  ylab = "self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")
       
        modREverSQF <- rep(-1, nrow(ddf))
        REverSQFdata <- as.numeric(vic$REverSQF)

        modREverSQF[REverSQFdata==1] <- 0
        modREverSQF[REverSQFdata==2] <- 1
        modREverSQF[REverSQFdata==3] <- 10
        modREverSQF[REverSQFdata==4] <- 2

        modREverSQF[REverSQFdata==5] <- 3
        modREverSQF[REverSQFdata==6] <- 4
        modREverSQF[REverSQFdata==7] <- 5
        modREverSQF[REverSQFdata==8] <- 6

        modREverSQF[REverSQFdata==9] <- 7
        modREverSQF[REverSQFdata==10] <- 8
        modREverSQF[REverSQFdata==11] <- 9
        modREverSQF[REverSQFdata==12] <- NA

        modREverSQF[REverSQFdata==13] <- 0
        modREverSQF[REverSQFdata==14] <- 1
        modREverSQF[REverSQFdata==15] <- 10
        modREverSQF[REverSQFdata==16] <- 2

        modREverSQF[REverSQFdata==17] <- 3
        modREverSQF[REverSQFdata==18] <- 4
        modREverSQF[REverSQFdata==19] <- 5
        modREverSQF[REverSQFdata==20] <- 6
 
        modREverSQF[REverSQFdata==21] <- 7 
        modREverSQF[REverSQFdata==22] <- 8
        modREverSQF[REverSQFdata==23] <- 9

        binSQFdata <- rep(-1, nrow(ddf))
        binSQFdata[is.na(modREverSQF)] <- NA
        binSQFdata[!is.na(modREverSQF) & modREverSQF==0] <- 1
        binSQFdata[!is.na(modREverSQF) & modREverSQF>0] <- 2

        binSQF <- factor(binSQFdata,levels=1:2,
                         labels = c("No","Yes"),
                         ordered = TRUE)
        vddf$binSQF <- binSQF

        fit <- aov(respVioIndex ~ binSQF,data=vddf)
        summary(fit)

        plotmeans(vddf$respVioIndex~vddf$binSQF,
                  xlab = "Does the Respondent report having been SQF",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        gunExpData <- rep(-1, nrow(ddf))
        gunSeenData <- as.numeric(vic$GunsSeenCount)

        gunExpData[is.na(gunSeenData)] <- NA

        gunExpData[gunSeenData==1] <- 0
        gunExpData[gunSeenData==2] <- 1
        gunExpData[gunSeenData==3] <- 10
        gunExpData[gunSeenData==4] <- 2

        gunExpData[gunSeenData==5] <- 3
        gunExpData[gunSeenData==6] <- 4
        gunExpData[gunSeenData==7] <- 5
        gunExpData[gunSeenData==8] <- 6

        gunExpData[gunSeenData==9] <- 7
        gunExpData[gunSeenData==10] <- 8
        gunExpData[gunSeenData==11] <- 9
        gunExpData[gunSeenData==12] <- NA

        vic$gunExp <- gunExpData

        vddf$gunExp<- gunExpData

       plotmeans(vddf$respVioIndex~vddf$gunExp,
                  xlab = "Leve of Gun Exposure",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        cor.test(x=vddf$gunExp,y=vddf$respVioIndex,alternative = c("two.sided"),method=c("pearson"))

        node <- data.frame(obs=1:nrow(df))
        
        node$age <- df$age                                  
        node$EdCompleted <- df$EdCompleted                          
        node$inSchool <- df$inSchool                             
        node$employed <- df$employed                             
        node$TimeOutsideHere <- df$TimeOutsideHere                      
        node$sleepTime <- df$sleepTime
        node$GangCommon <- df$GangCommon
        node$reportPolice <- df$reportPolice

        node <- node[,-1]
        ageData <- rep(-1,nrow(df))
        ageData[is.na(node$age)] <- NA
        for(i in 18:30) {
            ageData[as.numeric(node$age)==i-17] <- i
        }
        node$ageData <- ageData
        cor(x=node$ageData,y=vddf$respVioIndex,use="complete.obs",method=c("pearson"))
        plotmeans(vddf$respVioIndex~node$ageData,
                  xlab = "Level of age",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        edData <- rep(-1,nrow(df))
        edData[is.na(node$EdCompleted)] <- NA
        edData[as.numeric(node$EdCompleted)==1] <- 4
        edData[as.numeric(node$EdCompleted)==2] <- 5
        edData[as.numeric(node$EdCompleted)==3] <- 2
        edData[as.numeric(node$EdCompleted)==4] <- 1
        edData[as.numeric(node$EdCompleted)==5] <- 3
        
        node$Edmod <- factor(edData,levels=1:5, labels = c("lt. HS",
                                                    "Ged/HSd",
                                                    "Some Col",
                                                    "Assoc D",
                                                    "gt. Bach"),
                             ordered=TRUE)

        vddf$Edmod <- node$Edmod
        fit <- aov(respVioIndex ~ Edmod,data=vddf)
        summary(fit)

        barplot(prop.table(table(vddf$Edmod)),
                col=c("blue"),
                main="Distribution of education",
                xlab = "Education level",
                ylim = c(0,0.8),
                ylab = "probability"
                )

        plotmeans(vddf$respVioIndex~vddf$Edmod,
                  xlab = "Level of education",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        inSchoolData <- as.numeric(node$inSchool)-1
        inSchoolData[inSchoolData==0] <- NA
        vddf$inSchool <- factor(inSchoolData,
            ,levels = 1:2,
            labels=c("No","Yes"))

        fit <- aov(respVioIndex ~ inSchool,data=vddf)
        summary(fit)

        plotmeans(vddf$respVioIndex~vddf$inSchool,
                  xlab = "School Enrollment Status",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        empData <- as.numeric(node$employed)
        employed <- rep(-1,nrow(df))
        employed[is.na(empData)] <- NA
        employed[empData==1] <- 1
        employed[empData==2] <- NA
        employed[empData==3] <- 2
        employed[empData==4] <- 3

        vddf$employed <- factor(employed,levels = 1:3,
                                labels = c("full-time",
                                    "part-time",
                                    "unemployed"),
                                ordered=TRUE)

        fit <- aov(respVioIndex ~ employed,data=vddf)
        summary(fit)

        plotmeans(vddf$respVioIndex~vddf$employed,
                  xlab = "Employment Status",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        timeOut <- rep(-1,nrow(df))
        timeOut[is.na(node$TimeOutsideHere)] <- NA
        numTimeOut <- as.numeric(levels(node$TimeOutsideHere)[node$TimeOutsideHere])

        vddf$numTimeOut <- numTimeOut
        cor(x=vddf$numTimeOut,y=vddf$respVioIndex,use="complete.obs",method=c("pearson"))

        cor.test(x=vddf$numTimeOut,y=vddf$respVioIndex,alternative = c("two.sided"),method=c("pearson"))

        plotmeans(vddf$respVioIndex~vddf$numTimeOut,
                  xlab = "Level of Time Out",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        sleep <- rep(-1, nrow(df))
        sleep[is.na(node$sleepTime)] <- NA
        sleepChr <-levels(node$sleepTime)[as.numeric(node$sleepTime)]
       

                                        #evening
        sleep[sleepChr=="6 PM            "]<- 1  
        sleep[sleepChr=="7 PM            "]<- 1
        sleep[sleepChr=="8 PM            "]<- 1
        sleep[sleepChr=="9 PM            "]<- 1
                                        #night
        sleep[sleepChr=="10 PM           "]<- 2  
        sleep[sleepChr=="11 PM           "]<- 2
        sleep[sleepChr=="12 AM (Midnight)"]<- 2
        sleep[sleepChr=="1 AM            "]<- 2
                                        #late-night
        sleep[sleepChr=="2 AM            "]<- 3 
        sleep[sleepChr=="3 AM            "]<- 3
        sleep[sleepChr=="4 AM            "]<- 3
        sleep[sleepChr=="5 AM            "]<- 3
                                        #early-morn
        sleep[sleepChr=="6 AM            "]<- 4
        sleep[sleepChr=="7 AM            "]<- 4
        sleep[sleepChr=="8 AM            "]<- 4
        sleep[sleepChr=="9 AM            "]<- 4
                                        #late-morn        
        sleep[sleepChr=="10 AM           "]<- 5
        sleep[sleepChr=="11 AM           "]<- 5
        sleep[sleepChr=="12 PM (Noon)    "]<- 5
        sleep[sleepChr=="1 PM            "]<- 5
                                        #after-noon        
        sleep[sleepChr=="2 PM            "]<- 6
        sleep[sleepChr=="3 PM            "]<-6
        sleep[sleepChr=="4 PM            "]<-6
        sleep[sleepChr=="5 PM            "]<-6
        vddf$sleep <- factor(sleep,levels=1:6,
                             labels=c("evening",
                                 "night",
                                 "late-night",
                                 "early-morn",
                                 "late-morn",
                                 "after-noon"),
                             ordered=TRUE)
        

        fit <- aov(respVioIndex ~ sleep,data=vddf)
        summary(fit)
        plotmeans(vddf$respVioIndex~vddf$sleep,
                  xlab = "Sleep Time",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")

        gangData <- rep(-1,nrow(df))
        gangData[is.na(df$GangCommon)] <- NA
        gangData[levels(df$GangCommon)[1]==levels(df$GangCommon)[as.numeric(df$GangCommon)]] <- 1
        gangData[levels(df$GangCommon)[2]==levels(df$GangCommon)[as.numeric(df$GangCommon)]] <- 2
        gangData[levels(df$GangCommon)[3]==levels(df$GangCommon)[as.numeric(df$GangCommon)]] <- 3
        gangData[levels(df$GangCommon)[4]==levels(df$GangCommon)[as.numeric(df$GangCommon)]] <- 5
        gangData[levels(df$GangCommon)[5]==levels(df$GangCommon)[as.numeric(df$GangCommon)]] <- 4

        gangCommon <- factor(gangData,levels = 1:5,labels =
                             c("no-d","no-p","not-sure",
                               "yes-p","yes-d"),ordered=TRUE)

        vddf$gangCommon <- gangCommon
        fit <- aov(respVioIndex~gangCommon,data=vddf)
        summary(fit)
        library(gplots)
        plotmeans(vddf$respVioIndex~vddf$gangCommon,
                  xlab= "perceived gang prevelence",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")
                  
        reportData <- rep(-1,nrow(df))
        reportData[is.na(df$reportPolice)] <- NA
        reportData[levels(df$reportPolice)[1]==levels(df$reportPolice)[as.numeric(df$reportPolice)]] <- 1
        reportData[levels(df$reportPolice)[2]==levels(df$reportPolice)[as.numeric(df$reportPolice)]] <- 2
        reportData[levels(df$reportPolice)[3]==levels(df$reportPolice)[as.numeric(df$reportPolice)]] <- 3
        reportData[levels(df$reportPolice)[4]==levels(df$reportPolice)[as.numeric(df$reportPolice)]] <- 5
        reportData[levels(df$reportPolice)[5]==levels(df$reportPolice)[as.numeric(df$reportPolice)]] <- 4

        reportPolice <- factor(reportData,levels = 1:5,labels =
                             c("no-d","no-p","not-sure",
                               "yes-p","yes-d"),ordered=TRUE)

        vddf$reportPolice <- reportPolice
        fit <- aov(respVioIndex~reportPolice,data=vddf)
        summary(fit)
        library(gplots)
        plotmeans(vddf$respVioIndex~vddf$reportPolice,
                  xlab= "willingness to report crime",
                  ylab = "Self-reported propensity for violence",
                  main= "Mean plot with 95% confidence intervals")
                
