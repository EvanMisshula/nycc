library(foreign)
df1 <- read.spss(file = "~/Documents/city_council/EastNY.sav",to.data.frame = TRUE)
df2 <- read.spss(file = "~/Documents/city_council/Harlem.sav",to.data.frame = TRUE)
              


barplotVars <- c(8,10,12,14,16)
counts <- table(df[,8])
barplot(counts, main=varNames[8])

counts <- table(df[,10])
barplot(counts, main=varNames[10])

# Helped a neighbor


nR1 <- nrow(df1)
nR2 <- nrow(df2)


HelpedNeighbor1 <- rep(-1,nR1)
HelpedNeighbor2 <- rep(-1,nR2)




HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[1]==df1[,'HelpedNeighbor']] <- 2
HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[2]==df1[,'HelpedNeighbor']] <- 3
HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[3]==df1[,'HelpedNeighbor']] <- 4
HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[4]==df1[,'HelpedNeighbor']] <- 5
HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[5]==df1[,'HelpedNeighbor']] <- 5
HelpedNeighbor1[levels(df1[,'HelpedNeighbor'])[6]==df1[,'HelpedNeighbor']] <- 1


HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[1]==df2[,'HelpedNeighbor']] <- NA
HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[2]==df2[,'HelpedNeighbor']] <- 2
HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[3]==df2[,'HelpedNeighbor']] <- 3
HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[4]==df2[,'HelpedNeighbor']] <- 4
HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[5]==df2[,'HelpedNeighbor']] <- 5
HelpedNeighbor2[levels(df2[,'HelpedNeighbor'])[6]==df2[,'HelpedNeighbor']] <- 1
HelpedNeighbor <- c(HelpedNeighbor1,HelpedNeighbor2)
HelpedNeighbor <- factor(HelpedNeighbor,
                           levels = 1:5,
                           labels = c("lt 1 wk",
             h                 "btwn wk & mo",
                               "gt 1 mo",
                               "never",
                               "decline"
                               ),ordered=TRUE)

counts <- table(HelpedNeighbor)
barplot(counts, main='HelpedNeighbor',col="blue")


HelpedByNeighbor <- rep(-1,12)
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[1]==df[,'HelpedByNeighbor']] <- 5
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[2]==df[,'HelpedByNeighbor']] <- 2
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[3]==df[,'HelpedByNeighbor']] <- 3
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[4]==df[,'HelpedByNeighbor']] <- 4
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[5]==df[,'HelpedByNeighbor']] <- 5
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[6]==df[,'HelpedByNeighbor']] <- 1
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[7]==df[,'HelpedByNeighbor']] <- 5
HelpedByNeighbor[levels(df[,'HelpedByNeighbor'])[8]==df[,'HelpedByNeighbor']] <- 1
HelpedByNeighbor <- factor(HelpedByNeighbor,levels = 1:5,
                           labels = c("lt 1 wk",
                              "btwn wk & mo",
                               "gt 1 mo",
                               "never",
                               "decline"
                               ),ordered=TRUE)

counts <- table(HelpedByNeighbor)
barplot(counts, main='HelpedByNeighbor',col="blue")


numResp <- length(unique(df[12]))
nR <- nrow(df)

lastNeighborConflictTime <- rep(-1,nR)

lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[1]==df[,'lastNeighborConflictTime']] <- 5
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[2]==df[,'lastNeighborConflictTime']] <- 2
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[3]==df[,'lastNeighborConflictTime']] <- 3
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[4]==df[,'lastNeighborConflictTime']] <- 4
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[5]==df[,'lastNeighborConflictTime']] <- 5
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[6]==df[,'lastNeighborConflictTime']] <- 5
lastNeighborConflictTime[levels(df[,'lastNeighborConflictTime'])[7]==df[,'lastNeighborConflictTime']] <- 1

lastNeighborConflictTime <- factor(lastNeighborConflictTime,
                                  levels = 1:5,
                           labels = c(
                               "lt 1 wk",
                               "gt 1 wk lt 1 mo",
                               "gt 1 mo",
                               "Never",
                               "Other"
                               ),ordered=TRUE)
counts <- table(lastNeighborConflictTime)
barplot(counts, main='lastNeighborConflictTime',col="blue")

counts <- table(df[,14])
barplot(counts, main=varNames[14])

counts <- table(df[,16])
barplot(counts, main=varNames[16])

HeardPersonThreatenedWGunHereL12Count <- rep(-1,nR)
for(i in 1:10) {
    HeardPersonThreatenedWGunHereL12Count[df[,20]==as.character(i-1)] <- i
}
    HeardPersonThreatenedWGunHereL12Count[df[,20]=="10+"] <- 10
h.data <- HeardPersonThreatenedWGunHereL12Count
HeardPersonThreatenedWGunHereL12Count <- factor(h.data,
                                                levels = 1:11,
                                                labels = c(
                                                    as.character(0:9),
                                                    "10+"),
                                                ordered=TRUE)
 
                                          

counts <- table(HeardPersonThreatenedWGunHereL12Count)
barplot(counts, main=varNames[21])

HeardGunshotsHereL12Count <- rep(-1,nR)
for(i in 1:10) {
    HeardGunshotsHereL12Count[df[,21]==as.character(i-1)] <- i
}
    HeardGunshotsHereL12Count[df[,21]=="10+"] <- 10
h.data <- HeardGunshotsHereL12Count
HeardGunshotsHereL12Count <- factor(h.data,
                                                levels = 1:11,
                                                labels = c(
                                                    as.character(0:9),
                                                    "10+"),
                                                ordered=TRUE)
 
                                          

counts <- table(HeardGunshotsHereL12Count)
barplot(counts, main=varNames[21])


FriskedByPD <- rep(-1,nR)
for(i in 1:10) {
    FriskedByPD[df[,21]==as.character(i-1)] <- i
}
    FriskedByPD[df[,21]=="10+"] <- 10
h.data <- FriskedByPD
FriskedByPD <- factor(h.data,
                                                levels = 1:11,
                                                labels = c(
                                                    as.character(0:9),
                                                    "10+"),
                                                ordered=TRUE)
 
                                          

counts <- table(FriskedByPD)
barplot(counts, main=varNames[22])

counts <- table(df['RClubStepIn'])
barplot(counts, main=varNames[22])
#Scenarios

RClubStepIn <- rep(-1,nR)

RClubStepIn["Ignore"==df[,23]] <- 1
RClubStepIn["React verbally"==df[,23]] <- 2
RClubStepIn["Get Physical "==df[,23]] <- 3
RClubStepIn["Pull a weapon"==df[,23]] <- 4
RClubStepIn["Use a weapon"==df[,23]] <- 5
RClubStepIn["Not sure/Decline"==df[,23]] <- 6


RClubStepIn <- factor(RClubStepIn,
                      levels = 1:6,
                      labels = c(
                          "Ignore",
                          "VReact",
                          "PReact",
                          "DWeapon",
                          "UWeapon",
                          "Decline"
                          ),ordered=TRUE)
counts <- table(RClubStepIn)
barplot(counts, main=varNames[23])

#assign
rscore <- list()
rscore.h <- list()
rscore.e  <- list()
rscorer <- list()
scenarios <- list()
pctDecline <- list()
counts <- list()
counts.h <- list()
counts.e <- list()
pctRDecline <- list()
subt <- list()

for(j in 24:39) {
    scenarios[[varNames[j]]] <- rep(-1,nR)
    rscore[[varNames[j]]] <- -1
    rscorer[[varNames[j]]] <- -1
    pctRDecline[[varNames[j]]] <- -1
    counts[[varNames[j]]] <- rep(-1,nR)
    subt[[varNames[j]]] <- -1
    scenarios[[varNames[j]]] <- rep(-1,nR)
    cat(class(df[,j]),"\n",sep="")
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


for(j in (24:39)) {
    
    scenarios[[j-23]]["Ignore"==trim(levels(df[,j])[df[,j]])] <- 1
    scenarios[[j-23]]["React verbally"==trim(levels(df[,j])[df[,j]])] <- 2
    scenarios[[j-23]]["Get Physical"==trim(levels(df[,j])[df[,j]])] <- 3
    scenarios[[j-23]]["Pull a weapon"==trim(levels(df[,j])[df[,j]])] <- 4
    scenarios[[j-23]]["Use a weapon"==trim(levels(df[,j])[df[,j]])] <- 5
    scenarios[[j-23]]["Not sure/Decline"==trim(levels(df[,j])[df[,j]])] <- 6
    rscorer[[j-23]] <- median(scenarios[[j-23]][scenarios[[j-23]]!=6])
    rscore[[j-23]]  <- mean(scenarios[[j-23]][scenarios[[j-23]]!=6])
    rscore.h[[j-23]]<- mean(scenarios[[j-23]][scenarios[[j-23]]!=6 &
                                              df$location=="Harlem"])
    rscore.e[[j-23]]<- mean(scenarios[[j-23]][scenarios[[j-23]]!=6 &
                                              df$location=="EastNY"])

    pctRDecline[[j-23]] <- round(sum(scenarios[[j-23]]==6)/length(scenarios[[j-23]]),2)
    
    scenarios[[j-23]]<- factor(scenarios[[j-23]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
    counts[[j-23]] <- table(scenarios[[j-23]])
    counts.h[[j-23]]<- table(scenarios[[j-23]][df$location=="Harlem"])
    counts.e[[j-23]]<- table(scenarios[[j-23]][df$location=="EastNY"])
}

for(j in (24:39)) {
    jpeg(paste("scenario_graphs/",varNames[j],".jpg",sep = ""))
    subt[[j-23]] <- paste("r-score = ",
                  round(rscore[[j-23]],2),", ",
                  levels(scenarios[[j-23]])[rscorer[[j-23]]],", ",
                  "rank = ",17-(rank(unlist(rscore),ties.method = "random")[j-23]),
                  sep = "")
    cat(subt[[j-23]],"\n")                                     
    barplot(counts[[j-23]], main=varNames[j],col = "blue",sub=subt[[j-23]])
    dev.off()
}

medianAction <- levels(scenarios[[1]])[unlist(rscorer)[order(unlist(rscore),decreasing = TRUE)]]
scenes <- unlist(rscore)[order(unlist(rscore),decreasing = TRUE)]
scene_rank <- data.frame(scenarios=scenes,medianAction=medianAction)

write.table(scene_rank,file="scenario_graphs/rank.txt",sep="\t")

Fscenarios <- list()
fscore <- list()
fscore.h <- list()
fscore.e <- list()
fscorer <- list()
counts <- list()
pctFDecline <- list()
subt <- list()


for(j in 41:57) {
    Fscenarios[[varNames[j]]] <- rep(-1,nR)
    cat(class(df[,j]),"\n",sep="")
    fscore[[varNames[j]]] <- -1
    fscore.h[[varNames[j]]] <- -1
    fscore.e[[varNames[j]]] <- -1    
    fscorer[[varNames[j]]] <- -1
    subt[[varNames[j]]] <- -1        
    scenarios[[varNames[j]]] <- rep(-1,nR)
    counts[[varNames[j]]] <- rep(-1,6)
    cat(class(df[,j]),"\n",sep="")

}

for(j in (41:57)) {
    
    Fscenarios[[j-40]]["Ignore"==trim(levels(df[,j])[df[,j]])] <- 1
    Fscenarios[[j-40]]["React verbally"==trim(levels(df[,j])[df[,j]])] <- 2
    Fscenarios[[j-40]]["Get Physical"==trim(levels(df[,j])[df[,j]])] <- 3
    Fscenarios[[j-40]]["Pull a weapon"==trim(levels(df[,j])[df[,j]])] <- 4
    Fscenarios[[j-40]]["Use a weapon"==trim(levels(df[,j])[df[,j]])] <- 5
    Fscenarios[[j-40]]["Not sure/Decline"==trim(levels(df[,j])[df[,j]])] <- 6
    Fscenarios[[j-40]]["Not sure"==trim(levels(df[,j])[df[,j]])] <- 6
    Fscenarios[[j-40]]["Yes definitely"==trim(levels(df[,j])[df[,j]])] <- 6
    fscorer[[j-40]] <- median(Fscenarios[[j-40]][Fscenarios[[j-40]]!=6])
    fscore[[j-40]]  <- mean(Fscenarios[[j-40]][Fscenarios[[j-40]]!=6])
    fscore.h[[j-40]]<- mean(Fscenarios[[j-40]][Fscenarios[[j-40]]!=6 &
                                              df$location=="Harlem"])
    fscore.e[[j-40]]<- mean(Fscenarios[[j-40]][Fscenarios[[j-40]]!=6 &
                                              df$location=="EastNY"])
    pctFDecline[[j-40]] <- round(sum(Fscenarios[[j-40]]==6)/length(Fscenarios[[j-40]]),2)
    Fscenarios[[j-40]]<- factor(Fscenarios[[j-40]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
    counts[[j-40]] <- table(Fscenarios[[j-40]])
    counts.h[[j-40]]<- table(scenarios[[j-40]][df$location=="Harlem"])
    counts.e[[j-40]]<- table(scenarios[[j-40]][df$location=="EastNY"])
}
for(j in (41:57)) {
    pdf(paste("Fscenario_graphs/",varNames[j],".pdf",sep = ""))
    subt[[j-40]] <- paste("f-score = ",
                  round(fscore[[j-40]],2),", ",
                  levels(Fscenarios[[j-40]])[fscorer[[j-40]]],", ",
                  "rank = ",18-(rank(unlist(fscore),ties.method = "random")[j-40]),
                  sep = "")
    cat(j, " ",varNames[j]," ",subt[[j-40]],"\n")                                     
    barplot(counts[[j-40]], main=varNames[j],col = "black",sub=subt[[j-40]])
###    barplot(counts, main=varNames[j])
    dev.off()
}

medianAction <- levels(Fscenarios[[1]])[unlist(fscorer)[order(unlist(fscore),decreasing = TRUE)]]
scenes <- unlist(fscore)[order(unlist(fscore),decreasing = TRUE)]
scene_rank <- data.frame(scenarios=scenes,medianAction=medianAction)

write.table(scene_rank,file="Fscenario_graphs/rank.txt",sep="\t")


                                        # multiplot

rscore <- list()
fscore <- list()

scenarios <- list()
for(j in 24:39) {
   scenarios[[varNames[j]]] <- rep(-1,nR)
   rscore[[varNames[j]]] <- -1

   
}

for(j in (24:39)) {
    
    scenarios[[j-23]]["Ignore"==levels(df[,j])[df[,j]]] <- 1
    scenarios[[j-23]]["React verbally"==levels(df[,j])[df[,j]]] <- 2
    scenarios[[j-23]]["Get Physical "==levels(df[,j])[df[,j]]] <- 3
    scenarios[[j-23]]["Pull a weapon"==levels(df[,j])[df[,j]]] <- 4
    scenarios[[j-23]]["Use a weapon"==levels(df[,j])[df[,j]]] <- 5
    scenarios[[j-23]]["Not sure/Decline"==levels(df[,j])[df[,j]]] <- 6
    scenarios[[j-23]]["Not sure"==levels(df[,j])[df[,j]]] <- 6
    scenarios[[j-23]]["Yes definitely"==levels(df[,j])[df[,j]]] <- 6
                 
    rscore[[j-23]] <- mean(scenarios[[j-23]][scenarios[[j-23]]!=6])

    scenarios[[j-23]]<- factor(scenarios[[j-23]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
}

Rscore <- data.frame(rscore=unlist(rscore)[order(unlist(rscore),decreasing = TRUE)])
row.names(Rscore) <- names(rscore)[order(unlist(rscore),decreasing = TRUE)]

pdf("multi_scenario/respScenarios.pdf")
par(mfrow = c(4,4))
for(j in (24:39)) {
    counts <- table(scenarios[[j-23]])
    barplot(counts, main=varNames[j],col="blue",sub=paste("r-score=",
                                                    round(rscore[[j-23]],2)
                                                    ))

}
dev.off()


Fscenarios <- list()
fscore <- list()

for(j in 41:57) {
    Fscenarios[[varNames[j]]] <- rep(-1,nR)
    fscore[[varNames[j]]] <- -1
                        }
for(j in (41:57)) {
    
Fscenarios[[j-40]]["Ignore"          ==levels(df[,j])[df[,j]]] <- 1
Fscenarios[[j-40]]["React verbally"  ==levels(df[,j])[df[,j]]] <- 2
Fscenarios[[j-40]]["Get Physical "   ==levels(df[,j])[df[,j]]] <- 3
Fscenarios[[j-40]]["Pull a weapon"   ==levels(df[,j])[df[,j]]] <- 4
Fscenarios[[j-40]]["Use a weapon"    ==levels(df[,j])[df[,j]]] <- 5
Fscenarios[[j-40]]["Not sure/Decline"==levels(df[,j])[df[,j]]] <- 6
Fscenarios[[j-40]]["Not sure"==levels(df[,j])[df[,j]]] <- 6
Fscenarios[[j-40]]["Yes definitely"==levels(df[,j])[df[,j]]] <- 6

fscore[[j-40]] <- mean(Fscenarios[[j-40]][Fscenarios[[j-40]]!=6])


Fscenarios[[j-40]]<- factor(Fscenarios[[j-40]],
                          levels = 1:6,
                          labels = c(
                              "Ignore",
                              "VReact",
                              "PReact",
                              "DWeapon",
                              "UWeapon",
                              "Decline"
                              ),ordered=TRUE)
}
Fscore <- data.frame(fscore=unlist(fscore)[order(unlist(fscore),decreasing = TRUE)])
row.names(Fscore) <- names(fscore)[order(unlist(fscore),decreasing = TRUE)]


pdf("multi_scenario/friendScenarios.pdf")
par(mfrow = c(4,4))
friendRespScen <- c(47,44,48,55,
                    52,57,41,56,
                    50,46,42,49,
                    53,43,51,45)
                    
for(j in 1:length(friendRespScen)) {
    counts <- table(Fscenarios[[friendRespScen[j]-40]])
    barplot(counts, col="black",main=varNames[friendRespScen[j]],sub=paste("f-score=",round(unlist(fscore)[friendRespScen[j]-40],2)))
                                                    
                                                        
}
dev.off()

#friends and resp
for(j in 1:length(friendRespScen)) {
    rcounts <- table(scenarios[[j]])
    fcounts <- table(Fscenarios[[friendRespScen[j]-40]])
    
    data <- matrix(c(rcounts,fcounts),byrow = TRUE,nrow=2)
    row.names(data) <- c("respondents","friends")
    colnames(data) <- names(rcounts)

    pdf(paste("grouped_scenario/",varNames[j+23],".pdf",sep = ""))
    barplot(data,
            main=paste("Respondents vs. friends",varNames[j+23],sep=" "),
            col=c("darkblue","red"),
            legend = c("respondents","friends"),
            beside = TRUE)
    dev.off()
}
sleepData=rep(-1,nR)

sleepData[sleepTimeC=="7 PM"] <- 1
sleepData[sleepTimeC=="8 PM"] <- 2            
sleepData[sleepTimeC=="9 PM"] <- 3
sleepData[sleepTimeC=="10 PM"] <- 4
sleepData[sleepTimeC=="11 PM"] <- 5
sleepData[sleepTimeC=="12 AM (Midnight)"] <- 6
sleepData[sleepTimeC=="1 AM"] <- 7
sleepData[sleepTimeC=="2 AM"] <- 8
sleepData[sleepTimeC=="3 AM"] <- 9            
sleepData[sleepTimeC=="4 AM"] <- 10            

sleepLabels <- c("7p",
                 "8p",
                 "9p",
                 "10p",
                 "11p",
                 "12a",
                 "1a",
                 "2a",
                 "3a",
                 "4a")

                 
sleep <- factor(sleepData,levels = 1:10,labels = sleepLabels,ordered = TRUE)

jpeg("resp_marginals/sleep.jpg")
counts <- table(sleep)
barplot(counts, main="Sleep Time")


dev.off()
#gangs
gangData=rep(-1,nR)
rawGang <- df$GangCommon
gangData[rawGang=="Yes definitely"] <- 1
gangData[rawGang=="Yes probably"] <- 2            
gangData[rawGang=="Not sure"] <- 3
gangData[rawGang=="No probably "] <- 4
gangData[rawGang=="No definitely "] <- 5

gangPrev <- factor(gangData,levels = 1:5,
                     labels = c("Yes-d",
                         "Yes-p",
                         "Unsure",
                         "No-p",
                         "No-d")
                     ,ordered=TRUE)

jpeg("resp_marginals/gang.jpg")
counts <- table(gangPrev)
barplot(counts, main="Perceived Gang Prevelance")


dev.off()
