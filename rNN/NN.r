#implementation of a neural net for regression
#number of hidden neurons is  defined by mValue
#optional parameter lrate can be adjusted but is by default 0.01 
	NN <- function (
		trainObject
	, 	testObject
	, 	trainLabel
	,	testLabel
	#parameters
	, 	mValue
	,	lrate 	= 	0.01
	,	epoch 	= 	500
	#stopping rule
	,	srule 	= 	0
	#	for return statement
	,	output 	= 	0 #output == 0 return Y; output == 1 return MSE
	,	set		=	0 #set == 0 test ; set == 1 train 
	) {
	#SIGMOID FUNCTION
	sigmoid <- function (v) {
		return ((1+exp(-v))^-1);
		}
	#SIGMOID PRIME
	sigmoid.p <- function (v) {
		return(exp(v)*(exp(v)+1)^-2);
		}
	#INITIALISE DATA FRAMES
		df.trainObject 												<- data.frame(trainObject);
		df.testObject 												<- data.frame(testObject);
		df.trainLabel												<- data.frame(trainLabel);		
	#PARAMETERS
		#number of attributes
		p = ncol(df.trainObject);	
		#number of observations
		n = nrow(df.trainObject);
		#weights for alpha and beta are randomly generated in the region [-0.7 , 0.7]
		#create alpha matrix
		w.alpha														<- array(data=NA, dim=c(mValue,1+p));
		#assign weights for alpha		
		w.alpha[1:mValue,1:(1+p)]									<- apply(	data.frame(w.alpha)
																			,	2
																			,	function(x)
																				as.matrix(runif(mValue, min = -0.7, max = 0.7))
																			);
		#assign weights for beta.0 and beta.m
		w.beta.0													<- runif(1, min = -0.7, max = 0.7);
		w.beta.m													<- as.matrix(runif(mValue, min = -0.7, max = 0.7));	
	#trainMSE matrix for stopping rule
		trainMSE													<- array(data=0, dim=c(epoch));
#BEGIN EPOCH	
	for (i in 1:epoch) {
	#FORWARD PASS
		#dataframes for alpha and beta
		df.w.alpha													<- data.frame(1:mValue,w.alpha);
		df.w.beta.m													<- data.frame(1:mValue,w.beta.m);
		#calculate Z
		Z															<- apply(	df.w.alpha
																			,	1
																			,	function(x)	
																				sigmoid (
																					x[2] + 	as.matrix(trainObject) %*% as.matrix(x[(1:p)+2])
																				)
																			);																
		#calculate Y
		Y															<- w.beta.0 + (Z %*% w.beta.m);	
		#calculate trainMSE
		trainMSE[i]													<- sum((Y-trainLabel)^2)/nrow(Y);
		#stopping rule
		stopping = FALSE;
		if (i>10) {
		stopping													<- if(abs(trainMSE[(i-10)] - trainMSE[i]) < (srule * trainMSE[(i-10)])) {TRUE} else {FALSE}
		}
		if (stopping==TRUE) {			
			break;
			}
	#BACKWARD PASS
		#compute error
		delta														<- 2 *(df.trainLabel-Y);
		#back-propagation equation
		sim															<- apply(	df.w.alpha
																			,	1
																			,	function(x)																				
																				df.w.beta.m[x[1],2]
																			*	sigmoid.p (
																				x[2] + 	as.matrix(trainObject) %*% as.matrix(x[(1:p)+2])																				
																				)
																			);				
		sim															<- apply(	sim
																			,	2
																			,	function(x)
																				diag(as.matrix(delta) %*% t(x))
																			);														
	#UPDATE WEIGHTS
		#betas
		w.beta.0													<- w.beta.0 + lrate/n * sum(delta);	#since Z_i0 := 1
		w.beta.m													<- w.beta.m + (lrate/n * t(t(as.matrix(delta)) %*% as.matrix(Z)));
		#alphas
		w.alpha[1:mValue,1]											<- apply(	df.w.alpha
																			,	1
																			, 	function(x)
																				lrate/n*sum(sim[,x[1]])
																			);		
		w.alpha[1:mValue,-1]										<- apply(	df.w.alpha
																			,	1
																			,	function(x)
																				x[(1:p)+2] + (lrate/n * t(t(sim[,x[1]]) %*% as.matrix(trainObject)))
																			);																			
		}
#END EPOCH
	print(paste0("Stopping at epoch " , i));
	#FINAL MODEL
		#dataframes for alpha and beta
		df.w.alpha													<- data.frame(1:mValue,w.alpha);
		df.w.beta.m													<- data.frame(1:mValue,w.beta.m);
		#calculate Z
		if (set == 0) {
		Z															<- apply(	df.w.alpha
																			,	1
																			,	function(x)	
																				sigmoid (
																					x[2] + 	as.matrix(testObject) %*% as.matrix(x[(1:p)+2])
																				)
																			);		
		}
		else
		{
		Z															<- apply(	df.w.alpha
																			,	1
																			,	function(x)	
																				sigmoid (
																					x[2] + 	as.matrix(trainObject) %*% as.matrix(x[(1:p)+2])
																				)
																			);	
		}
		#calculate Y
		Y															<- w.beta.0 + (Z %*% w.beta.m);		
		if (set == 0) 
		MSE															<- sum((Y-testLabel)^2)/nrow(Y);
		if (set == 1)
		MSE															<- sum((Y-trainLabel)^2)/nrow(Y);
		
		if (output == 0) {return (Y)} else {return(MSE)};
}