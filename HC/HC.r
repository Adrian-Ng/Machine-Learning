#HIERARCHICAL CLUSTERING
#WITH SINGLE, COMPLETE, AVERAGE AND CENTROID LINKAGES
HC	<-	function 
	(	object
	,	linkage #0 == single; 1 == complete; 
	) {
	#EUCLIDEAN DISTANCE
		f.euclidean		<- function (x) {
			e.d 			<- apply(	x
									,	1
									,	function(a)
										apply(	x
											,	1
											,	function(b)
												sum((a-b)^2)
											)
									);
			return(e.d)
			};
	#LINKAGE FUNCTIONS
		#SINGLE	
		f.single 		<- function (d,w.0) {		
			min.d			<- cbind( 	apply( 	d
											, 	2
											, 	function(x) 
												which.min(x)
										)
									,	apply( 	d
											, 	2
											, 	function(x)
												min(x)
										));													
			w.1 			<- 	cbind(min.d[,2],w.0)
			w.2 			<-	aggregate(w.1[,2],by=list(w.1[,1]),FUN=min);			
			w	 			<- 	cbind(min.d,apply(min.d,1,function(x) w.2[w.2[,1]==x[2],2]))
			rm(min.d);
			rm(w.1);
			rm(w.2);
			return (w);
		}; 
		#COMPLETE	                  
		f.complete 		<- function (d,w.0) {
			max.d			<- cbind( 	apply( 	d
											, 	2
											, 	function(x) 
												which.max(x)
										)
									,	apply( 	d
											, 	2
											, 	function(x)
												max(x)
										));													
			w.1 			<- 	cbind(max.d[,2],w.0)
			w.2 			<-	aggregate(w.1[,2],by=list(w.1[,1]),FUN=max);			
			w	 			<- 	cbind(max.d,apply(max.d,1,function(x) w.2[w.2[,1]==x[2],2]))
			rm(max.d);
			rm(w.1);
			rm(w.2);
			return (w);
		};			
	#INITIALISE DATA FRAMES
		df.object 												<- data.frame(object);	
	#PARAMETERS
		#number of observations
		n = nrow(df.object);	
		#number of initial clusters
		n.w = n;
		#initial clusterings
		w.0 = 1:n;
		#create output array
		w.clusters												<- data.frame(w.0);
		w.distances												<- data.frame(matrix(NA, c(64,1)));
	#IF SINGLE LINKAGE
	if	(linkage == 0) {
		d														<- f.euclidean(df.object);
		d[d == 0]												<- Inf;
		while ( n.w > 1) {
			w													<- f.single(d,w.0);
			n.w													<- length(unique(w[,3]));
			#which clusters have grown?			
			w.grown												<- w[which(abs(w[,3] - w.0)>0),3];
			#temporarily set these to infinity
			for(i in 1:length(w.grown)) {				
				d[which(w[,3]==w.grown[i]),which(w[,3]==w.grown[i])] <- Inf
				};
			#what are the next smallest distances for these clusters?			
			for(j in 1:length(w.grown)) {				
				d[which(w[,3]==w.grown[j]),which(w[,3]==w.grown[j])]	<-	min(d[,which(w[,3]==w.grown[j])]);
				};
			#fix diagonal
			for (k in 1:n) {
				d[k,k] 											 <- Inf;
				};
			#store previous iteration's output
			w.clusters											<- cbind(w.clusters, w[,3]);
			w.distances											<- cbind(w.distances, w[,2]);			
			w.0													<- w[,3];			
			};
		};
	#IF COMPLETE LINKAGE	
	if	(linkage == 1) {
		d														<- f.euclidean(df.object);
		d[d == 0]												<- 0;
		while ( n.w > 1) {
			w													<- f.complete(d,w.0);
			n.w													<- length(unique(w[,3]));
			#which clusters have grown?			
			w.grown												<- w[which(abs(w[,3] - w.0)>0),3];
			#temporarily set these to zero
			for(i in 1:length(w.grown)) {				
				d[which(w[,3]==w.grown[i]),which(w[,3]==w.grown[i])] <- 0
				};
			#what are the next largest distances for these clusters?			
			for(j in 1:length(w.grown)) {				
				d[which(w[,3]==w.grown[j]),which(w[,3]==w.grown[j])]	<-	max(d[,which(w[,3]==w.grown[j])]);
				};
			#fix diagonal
			for (k in 1:n) {
				d[k,k] 											<- 0;
				};
			#store previous iteration's output
			w.clusters											<- cbind(w.clusters, w[,3]);
			w.distances											<- cbind(w.distances, w[,2]);
			w.0													<- w[,3];			
			};
		};	
		w.output												<- array(NA,dim=c(n,ncol(w.clusters),2));
		w.output[,,1]											<- as.matrix(w.clusters);
		w.output[,,2]											<- as.matrix(w.distances);
		return(w.output);
	};