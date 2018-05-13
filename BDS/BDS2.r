	#a function that generates decision stumps (DS) for 2 attributes
	BDS2 <- function (
		trainObject
	, 	testObject
	, 	trainLabel
	,	B = 1000 #default value
	,	lrate = 0.01 #default value
	)	{
	# RSS FUNCTION
	RSS	<- function (X,Y) {
			rss														<- sapply(	1:n
																		,	function(s)
																			sum((Y - mean(Y[which(X < X[s]),]))^2)
																		+	sum((Y - mean(Y[which(X[s] <= X),]))^2)
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
	#INITIALISE BOOSTING PARAMETERS		
		f.hat.x														<-	array(data = 0, dim=c(n,1));
		ri															<- 	df.trainLabel
		ri 															<- 	data.frame(ri);
#BEGIN BOOSTING
	for (i in 1:B) {
	#CALCULATE RSS		
	#RETURNS RSS FOR EACH CUTPOINT AND EACH ATTRIBUTE 	
		rss															<- 	apply( df.trainObject
																			,	2
																			,	function(x)
																				RSS(x,ri)
																			);
	#FOR WHICH CUTPOINT IS RSS MINIMIZED 
	#RETURNS A SINGLE MINIMIZED RSS FOR EACH ATTRIBUTE
		rss.min														<- 	apply( 	data.frame(rss)
																			,	2
																			,	function(x)
																				cbind(which.min(x),x[which.min(x)])																		
																			);	
		#1st row => index of RSS that contains minimized RSS for that attribute
		#2nd row => value of minimized RSS for that attribute
	#WHICH ATTRIBUTE 
		j															<-	which.min(rss.min[2,]);
	#CUTPOINT
		s 															<-	rss.min[1,which.min(rss.min[2,])];	
	#CALCULATE MEAN VALUE OF THE trainLabels that exist in r1 and r2
		r1.mean														<-	mean(ri[which(df.trainObject[,j] < df.trainObject[s,j]),])
		r2.mean														<-	mean(ri[which(df.trainObject[s,j] <= df.trainObject[,j]),])		
		
		f.hat.b														<- ifelse(df.testObject[,j] < df.trainObject[s,j],r1.mean,r2.mean);
		f.hat.x 													<- f.hat.x + f.hat.b;
		ri 															<- ri - (lrate * f.hat.b);	
		
#END BOOSTING
		}	
		f.hat.x 													<- lrate * f.hat.x
	return(f.hat.x);
			
	}
	