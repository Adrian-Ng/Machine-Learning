	#load function
	source("DA.r");

	#import iris data set
	iris <- read.table("iris.txt",header=F,sep=",")
	#separate data into objects and labels for preliminary analysis
	object <- iris[,-5];
	label <- iris[,5];

	#calculate means
	mean.0 <- data.frame(apply(object[(label==-1),],2,mean));
	mean.1 <- data.frame(apply(object[(label==1),],2,mean));
	#calculate stdev
	st.dev <- data.frame(apply(object,2,sd));
	#calculate difference
	View(data.frame(names(object),(mean.1-mean.0)/st.dev));
	
	#split data into train and test objects
	train.index <- sample(1:nrow(iris),70)
	train.iris <- iris[train.index,-5]
	test.iris <- iris[-train.index,-5]
	trainlabel.iris <- iris[train.index,5]
	testlabel.iris <- iris[-train.index,5]	
	
	#perform LDA and QDA with p = 1
	#LDA
	lda.p1 <- apply(data.frame(names(object)),1, function(x) DA(train.iris[,x],test.iris[,x],trainlabel.iris,0))
	match.lda.p1 <- apply(lda.p1,2, function(x) x==testlabel.iris)
	error.lda.p1 <- apply(match.lda.p1,2, function(x) 100-(sum(x)/length(x))*100)
	#QDA
	qda.p1 <- apply(data.frame(names(object)),1, function(x) DA(train.iris[,x],test.iris[,x],trainlabel.iris,0))
	match.qda.p1 <- apply(qda.p1,2, function(x) x==testlabel.iris)
	error.qda.p1 <- apply(match.qda.p1,2, function(x) 100-(sum(x)/length(x))*100)
	#return results
	View(data.frame(names(object),error.lda.p1,error.qda.p1))		
	
	#plot figure
	iris.label <- iris[,5]
	plot(iris[iris.label==-1,3],iris[iris.label==-1,4],type="p",col="red",xlim=c(1,6),ylim=c(0,2), xlab="V3", ylab="V4")
	
	
	#perform LDA and QDA for p = 2
	#LDA
	lda.p2 <- DA(train.iris[,c(3,4)], test.iris[,c(3,4)],trainlabel.iris,0)
	match.lda.p2 <- lda.p2 == testlabel.iris
	error.lda.p2 <- 100-(sum(match.lda.p2)/length(match.lda.p2))*100
	#QDA
	qda.p2 <- DA(train.iris[,c(3,4)], test.iris[,c(3,4)],trainlabel.iris,1)
	match.qda.p2 <- qda.p2 == testlabel.iris
	error.qda.p2 <- 100-(sum(match.qda.p2)/length(match.qda.p2))*100	
	#return results
	View(data.frame(error.lda.p2,error.qda.p2));
	
	#import Parkinson's data set
	park <- read.table("parkinsons.data", header=T, sep=",");	
	#separate data into objects and labels for preliminary analysis
	object <- park[,-c(1,18)];
	label <- park[,18];
	#calculate means
	mean.0 <- data.frame(apply(object[(label==0),],2,mean));
	mean.1 <- data.frame(apply(object[(label==1),],2,mean));
	#calculate stdev
	st.dev <- data.frame(apply(object,2,sd));
	#calculate difference
	View(data.frame(names(object),(mean.1-mean.0)/st.dev));

	#split data into train and test objects
	train.index <- sample(1:nrow(park),120)
	train.park <- park[train.index,-18]
	test.park <- park[-train.index,-18]
	trainlabel.park <- park[train.index,18]
	testlabel.park <- park[-train.index,18]

	#perform LDA and QDA with p = 1
	#LDA
	lda.p1 <- apply(data.frame(names(object)),1, function(x) DA(train.park[,x],test.park[,x],trainlabel.park,0))
	match.lda.p1 <- apply(lda.p1,2, function(x) x==testlabel.park)
	error.lda.p1 <- apply(match.lda.p1,2, function(x) 100-(sum(x)/length(x))*100)
	#QDA
	qda.p1 <- apply(data.frame(names(object)),1, function(x) DA(train.park[,x],test.park[,x],trainlabel.park,0))
	match.qda.p1 <- apply(qda.p1,2, function(x) x==testlabel.park)
	error.qda.p1 <- apply(match.qda.p1,2, function(x) 100-(sum(x)/length(x))*100)
	#return results
	View(data.frame(names(object),error.lda.p1,error.qda.p1))	
	
	#plot all 5 interesting attributes against each other
	object.p2 <- object[,c(1,3,19,20,22)]
	par(mfrow=c(5,4))
		for (i in 1:ncol(object.p2)) {
		  for (j in 1:ncol(object.p2)) {
			if (!i == j) {
				plot(object.p2[(label==0),i],object.p2[(label==0),j],type="p",col="red",xlab=paste(names(object.p2)[i]),ylab=paste(names(object.p2)[j]))
				   points(object.p2[(label==1),i],object.p2[(label==1),j],type="p",col="blue")
			}
		  }
		}
			
	#plot spread1 vs PPE
	par(mfrow=c(1,1))	
	plot(object.p2[(label==0),3],object.p2[(label==0),5],type="p",col="red",xlab=paste(names(object.p2)[3]),ylab=paste(names(object.p2)[5]))
	points(object.p2[(label==1),3],object.p2[(label==1),5],type="p",col="blue")
	
	#create train and test sets for p = 2 (same sample is used as before)
	train.park.p2 <- object.p2[train.index,]
	test.park.p2 <- object.p2[-train.index,]
	
	#pairing with MDVP.Fo.Hz
	#perform lda for p = 2	
	lda.p2 			<- apply(data.frame(names(object.p2[,-1])),1, function(x) DA(data.frame(train.park.p2[,1],train.park.p2[,x]),data.frame(test.park.p2[,1],test.park.p2[,x]),trainlabel.park,0))
	match.lda.p2 	<- apply(lda.p2,2, function(x) x==testlabel.park)
	error.lda.p2 	<- apply(match.lda.p2,2, function(x) 100-(sum(x)/length(x))*100)
	#perform qda for p = 2	
	qda.p2 			<- apply(data.frame(names(object.p2[,-1])),1, function(x) DA(data.frame(train.park.p2[,1],train.park.p2[,x]),data.frame(test.park.p2[,1],test.park.p2[,x]),trainlabel.park,1))
	match.qda.p2 	<- apply(qda.p2,2, function(x) x==testlabel.park)
	error.qda.p2 	<- apply(match.qda.p2,2, function(x) 100-(sum(x)/length(x))*100)
	#return results
	View(data.frame(names(object.p2[-1]),error.lda.p2,error.qda.p2))
	
	#pairing with spread1
	#perform lda for p = 2	
	lda.p2 			<- apply(data.frame(names(object.p2[,-3])),1, function(x) DA(data.frame(train.park.p2[,3],train.park.p2[,x]),data.frame(test.park.p2[,3],test.park.p2[,x]),trainlabel.park,0))
	match.lda.p2 	<- apply(lda.p2,2, function(x) x==testlabel.park)
	error.lda.p2 	<- apply(match.lda.p2,2, function(x) 100-(sum(x)/length(x))*100)
	#perform qda for p = 2	
	qda.p2 			<- apply(data.frame(names(object.p2[,-3])),1, function(x) DA(data.frame(train.park.p2[,3],train.park.p2[,x]),data.frame(test.park.p2[,3],test.park.p2[,x]),trainlabel.park,1))
	match.qda.p2 	<- apply(qda.p2,2, function(x) x==testlabel.park)
	error.qda.p2 	<- apply(match.qda.p2,2, function(x) 100-(sum(x)/length(x))*100)
	#return results
	View(data.frame(names(object.p2[-3]),error.lda.p2,error.qda.p2))
	
	
	
	
	
	
	