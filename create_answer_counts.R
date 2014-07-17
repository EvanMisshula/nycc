create_answer_count <- function(ddf){
    answer_count <- matrix(-1,nrow=36*16^2,ncol=5)
    colnames(answer_count) <- c("i","j","k","l","num_rec")
    ii <- 1
    cat(file="answer_count.csv","\n",append=FALSE)
    for(i in 1:16) {
        for(j in 1:16) {
            for(k in 1:6) {
                for(l in 1:6) {
                    num_records <- nrow(ddf[as.numeric(ddf[,i])==k &
                                            as.numeric(ddf[,j])==l,])
                    cat(file="answer_count.csv",'i=',i,
                        ", j=",j,
                        ", k=",k,
                        ", l=",l,
                        ", num_records=",num_records,"\n",append=TRUE,sep="")
                    answer_count[ii,] <- c(i,j,k,l,num_records)
                    ii <- ii+1
                }
            }
        }
    }
    
    return(answer_count)
}
