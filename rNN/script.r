	#load function
	source("NN.r");
	
	#load library
	library(ISLR);
	
	#load data )
	data(Auto);
	auto <- Auto;
	rm(Auto);
	#generate 50/50 random sample
	set.seed(1127);
	train.index <- sample(1:nrow(auto),nrow(auto)/2);
	
	#create dummy attributes to qualitative attribute origin
	isAmerican <- ifelse(auto$origin == 1,1,0)
	isEuropean <- ifelse(auto$origin == 2,1,0)
	isJapanese <- ifelse(auto$origin == 3,1,0)	
	
	#generate train and test sets
	train.auto <- data.frame(auto[,c(4,5,7)],isAmerican,isEuropean,isJapanese)[train.index,]
	test.auto <- data.frame(auto[,c(4,5,7)],isAmerican,isEuropean,isJapanese)[-train.index,]
	trainlabel.auto <-	auto[train.index,c("mpg")]
	testlabel.auto <- auto[-train.index,c("mpg")]
	#normalize attributes
	
	#TASK 4 vary M
	# testMSE for M hidden neurons 1:20
	testMSE.M <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	mValue = x
							,	output=1
							,	set=0)
						);
	# trainMSE for M hidden neurons 1:20
	testMSE.M <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	mValue = x
							,	output=1
							,	set1)
						);	
	View(data.frame(testMSE.M,trainMSE.M));							
	#TASK 4 vary eta
	# testMSE for M = 6; eta 0.001:0.02
	testMSE.eta <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	mValue = 6
							,	lrate = x*0.001
							,	output=1
							,	set=0)
						);						
	# trainMSE for M = 6; eta 0.001:0.02
	trainMSE.eta <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	mValue = 6
							,	lrate = x*0.001
							,	output=1
							,	set=1)
						);
	View(data.frame(testMSE.etc,trainMSE.etc));						
	#TASK 4 vary epoch
	# testMSE for M = 6; eta 0.01
	testMSE.epoch <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	6
							,	epoch = x * 50
							,	output=1
							,	set=0)
						);		
	# trainMSE for M = 6; eta 0.01
	trainMSE.epoch <- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto)
							,	scale(test.auto)
							, 	auto[train.index,c("mpg")]
							,	auto[-train.index,c("mpg")]
							,	mValue = 6
							,	epoch = x * 50
							,	output=1
							,	set=1)
						);
	View(data.frame(testMSE.epoch,trainMSE.epoch));
	#TASK 5 1% stopping rule
	testMSE.stop	 <- apply(data.frame(1:20)
					,	1
					,	function(x)
						NN(	scale(train.auto)
						,	scale(test.auto)
						, 	auto[train.index,c("mpg")]
						,	auto[-train.index,c("mpg")]
						,	mValue = 6
						, 	srule = 0.01
						, 	epoch=1000
						,	output=1
						,	set=0)
						);
	#TASK 5 0.1% stopping rule
	testMSE.stop	 <- apply(data.frame(1:20)
					,	1
					,	function(x)
						NN(	scale(train.auto)
						,	scale(test.auto)
						, 	auto[train.index,c("mpg")]
						,	auto[-train.index,c("mpg")]
						,	mValue = 6
						, 	srule = 0.001
						, 	epoch=1000
						,	output=1
						,	set=0)
						);	
	#TASK 6 CROSS VALIDATION
	folds <- cut(seq(1,nrow(train.auto)),breaks=4,labels=FALSE)
	trainlabel.auto <- auto[train.index,c("mpg")]
	testlabel.auto <- auto[-train.index,c("mpg")]	
	cv.testMSE.1 	<- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto[(folds!=1),])
							,	scale(train.auto[folds==1,])
							, 	trainlabel.auto[(folds!=1)]
							,	testlabel.auto[(folds==1)]
							,	mValue = x
							,	srule = 0.01
							,	lrate = 0.01
							,	output=1
							,	set=0)
						);
	cv.testMSE.2 	<- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto[(folds!=2),])
							,	scale(train.auto[folds==2,])
							, 	trainlabel.auto[(folds!=2)]
							,	testlabel.auto[(folds==2)]
							,	mValue = x
							,	srule = 0.01
							,	lrate = 0.01
							,	output=1
							,	set=0)
						);	
	cv.testMSE.3 	<- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto[(folds!=3),])
							,	scale(train.auto[folds==3,])
							, 	trainlabel.auto[(folds!=3)]
							,	testlabel.auto[(folds==3)]
							,	mValue = x
							,	srule = 0.01
							,	lrate = 0.01
							,	output=1
							,	set=0)
						);	
	cv.testMSE.4 	<- apply(data.frame(1:20)
					,	1
					,	function(x)
							NN(	scale(train.auto[(folds!=4),])
							,	scale(train.auto[folds==4,])
							, 	trainlabel.auto[(folds!=4)]
							,	testlabel.auto[(folds==4)]
							,	mValue = x
							,	srule = 0.01
							,	lrate = 0.01
							,	output=1
							,	set=0)
						);	
	View(data.frame(cv.testMSE.1,cv.testMSE.2,cv.testMSE.3,cv.testMSE.4));
	#TASK 7
	#train MSE for linear regression
	lm.fit <- lm(mpg~horsepower+weight+year+origin,data=auto, subset=train.index)
	train.lm.pred <- predict(lm.fit,auto[train.index,])
	(sum(train.lm.pred - trainlabel.auto)^2)/196
	#test MSE for linear regression
	test.lm.pred <- predict(lm.fit,auto[-train.index,])
	(sum(test.lm.pred - testlabel.auto)^2)/196
	#TASK 8
	MSE.100 <- array(NA,100);
	for (i in 1:100) {
	MSE.100[i] <- 		NN(	scale(train.auto)
						,	scale(test.auto)
						, 	auto[train.index,c("mpg")]
						,	auto[-train.index,c("mpg")]
						,	mValue = 6
						, 	srule = 0.001
						, 	epoch= 1000
						,	output=1
						,	set=0)
	}
	boxplot(MSE.100)