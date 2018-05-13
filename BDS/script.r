	#load data
	library(MASS);
	data("Boston");	
	source("BDS1.r")
	source("BDS2.r")	
	#split data
	#set.seed(1127);
	train.index 			<- sample(1:nrow(Boston),nrow(Boston)/2);
	#objects	
	train.boston 			<- Boston[train.index,c(6,13)];
	test.boston 			<- Boston[-train.index,c(6,13)];
	#labels	
	train.label.boston 		<- Boston[train.index,c(14)];
	test.label.boston 		<- Boston[-train.index,c(14)];
	#1 Train your DS implementation on the training set. Find the MSE on the test set.
	
		#return results for DS
		ds.result 			<- BDS1(train.boston,test.boston,train.label.boston);		
		#calculate test MSE
		mean((ds.result - test.label.boston)^2);
			#90.74128
	#2 Train your BDS implementation on the training set. Find the MSE on the test set.		
		#return results for BDS
		#function assumes learning rate of 0.01 as default
		bds.result 			<- BDS2(train.boston,test.boston,train.label.boston, B=1000);
		#calculate test MSE
		mean((bds.result - test.label.boston)^2)
			#90.71489
	#3 Plot the test MSE for a fixed value of eta. Iterate through B in the interval [1,B_0]. 
		# i want to first of all know which value of eta to use. Using B = 1000, I will try a number of values of eta.
		bds.MSE.eta 		<- sapply(	1:20
									,	function(eta)
										mean((BDS2(train.boston,test.boston,train.label.boston, B=1000,lrate=0.001*eta) - test.label.boston)^2)
									);									
	
		which.min(bds.MSE.eta);
			#8
			#this means eta = 0.08
			
			#incrementing B from 50 to 10,000 in steps of 50
		bds.testMSE.b_0	<- sapply(	1:20
									,	function(B_0)
										mean((BDS2(train.boston,test.boston,train.label.boston, B=B_0*500,lrate=0.08) - test.label.boston)^2)
									);
		x = (1:20)*500
		plot(x,bds.testMSE.b_0,xlab="B in the interval 50 to 10,000 in steps of 50",ylab="BDS test MSE",main="BDS with eta = 0.08",col='blue',type='b',ylim=c(50,300),xaxt="n");
		at <- seq(from=0,to=10000,by=1000);
		axis(side=1,at=at);
