	#a function that generates decision stumps (DS) for 2 attributes
	BDS1 <- function (
		trainObject
	, 	testObject
	, 	trainLabel
	)	{
	# RSS FUNCTION
	RSS	<- function (X,Y) {
			rss														<- sapply(	1:n
																		,	function(s)
																			sum((Y - mean(Y[which(X < X[s])]))^2)
																		+	sum((Y - mean(Y[which(X[s] <= X)]))^2)
																	);
			return(rss);
	};
	#INITIALISE DATA FRAMES
		df.trainObject 												<- 	data.frame(trainObject);
		df.testObject 												<- 	data.frame(testObject);
		df.trainLabel												<- 	data.frame(trainLabel);		
	#PARAMETERS
		#number of observations
		n															<- 	nrow(df.trainObject);
		#number of attributes	
		p															<- 	ncol(df.trainObject);
	#CALCULATE RSS		
	#RETURNS RSS FOR EACH CUTPOINT AND EACH ATTRIBUTE 	
		rss															<- 	apply( df.trainObject
																			,	2
																			,	function(x)
																				RSS(x,trainLabel)																				
																			);																
	#FOR WHICH CUTPOINT IS RSS MINIMIZED?
	#RETURNS A SINGLE MINIMIZED RSS FOR EACH ATTRIBUTE
		rss.min														<- 	apply( data.frame(rss)
																			,	2
																			,	function(x)
																				cbind(which.min(x),x[which.min(x)])																		
																			);	
		#print(rss.min);
		#1st row => index of RSS that contains minimized RSS for that attribute
		#2nd row => value of minimized RSS for that attribute
	#WHICH ATTRIBUTE 
		j															<-	which.min(rss.min[2,]);
	#CUTPOINT
		s 															<-	rss.min[1,which.min(rss.min[2,])];
	#CALCULATE MEAN VALUE OF THE trainLabels that exist in r1 and r2
		r1.mean														<-	mean(df.trainLabel[which(df.trainObject[,j] < df.trainObject[s,j]),])
		r2.mean														<-	mean(df.trainLabel[which(df.trainObject[s,j] <= df.trainObject[,j]),])
		
	#PREDICT testLabel BASED ON WHICH REGION THE testObjects FALL INTO
	#RETURN THE MEAN VALUE OF THESE REGIONS AS THE PREDICTION
		prediction 													<-	ifelse(df.testObject[,j] < df.trainObject[s,j],r1.mean,r2.mean);
		return(prediction);	
			
	}
	