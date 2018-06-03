library(rpart)
ecoli.df = read.csv("//svrin000egl01.asia.corp.anz.com/swamyy1$/Desktop/ML/CASE_STUDY/CASE_STUDY/Decision_Trees/ecoli/ecoli.txt")

ecoli.rpart1 = rpart(class ~ mcv + gvh + lip + chg + aac + alm1 + alm2,   data = ecoli.df)

plotcp(ecoli.rpart1)

printcp(ecoli.rpart1)

ecoli.rpart2 = prune(ecoli.rpart1, cp = 0.02)

plot(ecoli.rpart2, uniform = TRUE)
text(ecoli.rpart2, use.n = TRUE, cex = 0.75)