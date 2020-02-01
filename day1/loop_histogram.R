pdf("scores_hist.pdf", width=10, height=10)
par(mfrow=c(2,2), mar=c(5,4,4,2), oma=c(1,1,1,1))

for (i in 1:4){
  x <- scores[,i]
  hist(x,
      main=paste("Question ", i), 
      xlab="Scores",
      xlim=c(0, 10))
}
