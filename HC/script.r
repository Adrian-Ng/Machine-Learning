	#IMPORT DATA AND TRANSPOSE
	nci 		<- t(read.table("nci.data.txt"));
	#IMPORT LABELS
	label		<- read.table("label.txt");
	
	table(label);
	
	#SINGLE LINKAGE
	w.output	<- HC(nci,0);
	#The following provides a view on the cluster labels through iterations
	View(w.output[,,1]);
	#The following provides a view on the min distances between clusters but I could not find a way to incorporate these data in my report
	View(w.output[,,2]);
	#Find the number of clusters at each stage
	no.clusters <- apply(w.output[,,1],2,function(x) length(unique(x)));
	which(no.clusters==14);
		#There are 14 clusters at the 25th iteration
	#compare the result at the 14th iteration vs the label
	error <- data.frame(w.output[,25,1],label)
	View(error)
	#COMPLETE LINKAGE
	w.output	<- HC(nci,1);
	#The following provides a view on the cluster labels through iterations
	View(w.output[,,1]);
	#The following provides a view on the max distances between clusters
	View(w.output[,,2]);
	#Find the number of clusters at each stage
	no.clusters <- apply(w.output[,,1],2,function(x) length(unique(x)))	;
	which(no.clusters==14);
		#There are 14 clusters at the 51st iteration
	#compare the result at the 14th iteration vs the label
	error <- data.frame(w.output[,51,1],label)
	View(error)	
	#K-MEANS CLUSTERING
	km.nci  <- kmeans(nci,14,nstart=20);
	km.result <-km.nci$cluster;
	data.frame(km.result,label);