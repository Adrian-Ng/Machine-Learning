	#load KRR implementation
	# note that km = 0 polynomial; km = 1 radial; km = 2 for linear
	source("KRR.r");
	#load data
	ad 				<- read.csv("Advertising.csv");
	ad				<- ad[,-1];
	#split into test and training sets
	set.seed(1127);
	train.index 	<- sample(1:nrow(ad),nrow(ad)*2/3);
	train.ad		<- ad[train.index,-4];
	test.ad			<- ad[-train.index,-4];
	train.ad.labels	<- ad[train.index,4];
	test.ad.labels	<- ad[-train.index,4];	
	# CROSS VALIDATION FOLDS
	folds 			<- cut(seq(1,nrow(train.ad)),breaks=5,labels=FALSE);
	# CROSS VALIDATION WITH LINEAR KERNEL FOR OPTIMAL LAMBDA
	cv.trainMSE		<- 	sapply (	1:5
								,	function(f) #f for folds								
									sapply(	1:1000
										,	function(x)	#x for lambda							
											mean((train.ad.labels[(folds==f)] - KRR(	train.ad[(folds!=f),] #train on k-1 folds
																					,	train.ad[(folds==f),]
																					,	train.ad.labels[(folds!=f)] #k-1 training labels
																					, 	km = 2
																					, 	lambda = x
																					,	parameter = 0))^2)		
											)
								);
	which.min(rowMeans(cv.trainMSE));
	#plot figure 1
	plot(1:1000,rowMeans(cv.trainMSE), xlab="lambda",ylab="mean CV MSE",type="l",main="KRR on linear kernel")
		#optimal lambda = 231
	mean(cv.trainMSE[231,]);
		#4.523625
	# CROSS VALIDATION WITH POLYNOMIAL KERNEL FOR OPTIMAL d
	cv.trainMSE.p		<- 	sapply (	1:5
								,	function(f) #f for folds								
									sapply(	1:3
										,	function(d)		#d for dimension			
											mean((train.ad.labels[(folds==f)] - KRR(	train.ad[(folds!=f),] #train on k-1 folds
																					,	train.ad[(folds==f),]
																					,	train.ad.labels[(folds!=f)] #k-1 training labels
																					, 	km = 0
																					, 	lambda = 231
																					,	parameter = d))^2)		
											)
								);
	#	TEST MSE WITH VARYING d
	test.MSE.p			<-	sapply(	1:3
										,	function(d)					
															mean((test.ad.labels - KRR(	train.ad
																					,	test.ad
																					,	train.ad.labels
																					, 	km = 0
																					, 	lambda = 231
																					,	parameter = d))^2)		
											);
	
	#plot figure 2
	plot(1:3,test.MSE.p, xlab="d",ylab="MSE",type="l",main="KRR on polynomial kernel",ylim=c(0.5,3.5));
	points(1:3, rowMeans(cv.trainMSE.p),type="l",col="red");
	legend(2.5,3.5,c("test MSE","CV MSE"),lty=c(1,1), lwd=c(2.5,2.5),col=c("black","red"));
	
	min(cv.trainMSE.p);
		#0.2360326
	min(test.MSE.p);
		#1.242292
	
		# CROSS VALIDATION WITH RADIAL KERNEL FOR OPTIMAL lambda
	cv.trainMSE.r		<- 	sapply (	1:5
								,	function(f) #f for folds								
									sapply(	1:1000
										,	function(l)	#g for gamma				
											mean((train.ad.labels[(folds==f)] - KRR(	train.ad[(folds!=f),] #train on k-1 folds
																					,	train.ad[(folds==f),]
																					,	train.ad.labels[(folds!=f)] #k-1 training labels
																					, 	km = 1
																					, 	lambda = l
																					,	parameter = 1*10^-4))^2)		
											)
								);
								
	#	TEST MSE WITH VARYING d
	test.MSE.r 			<-	sapply(	1:1000
										,	function(l)					
															mean((test.ad.labels - KRR(	train.ad
																					,	test.ad
																					,	train.ad.labels
																					, 	km = 1
																					, 	lambda = l
																					,	parameter = 1*10^-4))^2)		
											);		

	#plot figure 3
	plot(1:1000, test.MSE.r, xlab="lambda",ylab="MSE",type="l",main="KRR on radial kernel");
	points(1:1000, rowMeans(cv.trainMSE.r),type="l",col="red");
	legend(800,25,c("test MSE","CV MSE"),lty=c(1,1), lwd=c(2.5,2.5),col=c("black","red"));	
	
	# CROSS VALIDATION WITH RADIAL KERNEL FOR OPTIMAL gamma
	cv.trainMSE.r		<- 	sapply (	1:5
								,	function(f) #f for folds								
									sapply(	1:10
										,	function(g)	#g for gamma				
											mean((train.ad.labels[(folds==f)] - KRR(	train.ad[(folds!=f),] #train on k-1 folds
																					,	train.ad[(folds==f),]
																					,	train.ad.labels[(folds!=f)] #k-1 training labels
																					, 	km = 1
																					, 	lambda = 1
																					,	parameter = 1*10^-g))^2)		
											)
								);
								
	#	TEST MSE WITH VARYING d
	test.MSE.r 			<-	sapply(	1:10
										,	function(g)					
															mean((test.ad.labels - KRR(	train.ad
																					,	test.ad
																					,	train.ad.labels
																					, 	km = 1
																					, 	lambda = 1
																					,	parameter = 1*10^-g))^2)		
											);		

	#plot figure 4
	plot(1:10, test.MSE.r, xlab="gamma = 1*10^-x",ylab="MSE",type="l",main="KRR on radial kernel");
	points(1:10, rowMeans(cv.trainMSE.r),type="l",col="red");
	legend(8,200,c("test MSE","CV MSE"),lty=c(1,1), lwd=c(2.5,2.5),col=c("black","red"));		


	
	min(cv.trainMSE.r);
		#1.134312
	min(test.MSE.r);
		#2.358666