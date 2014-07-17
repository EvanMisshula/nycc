create_distance_matrix(ddf,answer_count)
{
    nS <- ncol(ddf)
    nR <- nrow(ddf)
    sm <- matrix(0,nrow=nR,ncol=nR)
    for(i in 1:nR) {
        for(j in 1:nR) {
            for(k in 1:nS) {
                for(l in 1:nS) {
                    row_answer_1 <- ddf[i,k]
                    row_answer_2 <- ddf[i,l]
                    col_answer_1 <- ddf[j,k]
                    col_answer_2 <- ddf[j,l]
                    if(row_answer_1==col_answer_1 &&
                       row_answer_2==col_answer_2) {
                        sm[i,j] <- sm[i,j]+(1/answer_count[
                                                          ,5 ])
                    }
                }
            }
        }
    }
    return(sm)
}
