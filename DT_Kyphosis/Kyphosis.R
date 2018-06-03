Let's use the data frame kyphosis to predict a type of deformation (kyphosis) after surgery, from age in months (Age), number of vertebrae involved (Number), and the highest vertebrae operated on (Start).

# Classification Tree with rpart
library(rpart)

head(kyphosis)
summary(kyphosis)
colnames(kyphosis)

# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start,
  	method="class", data=kyphosis)
	
printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
  	main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/DT_Kyphosis/tree.ps", 
  	title = "Classification Tree for Kyphosis")
	
	# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
  	main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "C:/Users/Pandu/Desktop/Pandu_ course/CASE_STUDY/DT_Kyphosis/tree.ps", 
  	title = "Pruned Classification Tree for Kyphosis")
	


	
	

