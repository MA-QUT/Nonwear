

#'The sample data has been converted to 1 sec windows with the SD of the vector magnitude
#'The sample code can be adapted for different window legnths and different lengths of time to be considered non wear
#'Final output will be a vector of 1 and 0. 1 = non wear, 0 = wear


url<-RCurl::getURLContent("https://raw.githubusercontent.com/MA-QUT/Nonwear/master/sample_nonwear_data.csv")
sample<-data.table::fread(url)  

  window<-1 # designate window length (in seconds) that was used to calcualte SD

  vm<-rep(0,nrow(sample))
  e<-which(sample$sdvm<0.013) #SD threshold
  vm[e]<-1
  run<-rle(vm)
  run<-rep(run$lengths,run$lengths)
  vm<-cbind(vm,run)
  
  e<-which(vm[,1]==1 & vm[,2]>=60/window*30) #index all values where SD is below threshold for at least 30 minutes
  nonwear<-rep(0,nrow(vm))
  nonwear[e]<-1
  
  check<-nonwear
  run<-rle(check)
  run<-cbind(run[[1]],run[[2]])
  
  
  i=1
  ii=1
  wp<-matrix(0,length(which(run[,2]==0)),3) #create a table with row length equal to wear periods
  while(i<=nrow(run)){
    if(run[i,2]==0){
      if(i>1){wp[ii,1]<-run[i-1,1]} #non-wear before wear period
      wp[ii,2]<-run[i,1] #wear period
      if(i<nrow(run)){wp[ii,3]<-run[i+1,1]} #non-wear after wear period
      
      if (wp[ii, 2] < ((60/window*30)) & (wp[ii,2]/(wp[ii, 1] + wp[ii, 3])) < 0.3) {
        run[i,2] = 1 
        #' if wear period is less than 30 min and less than 30% of bordering non-wear period,
        #' convert to nonwear
      }
      ii<-ii+1
    }
    i<-i+1
  }
  check<-rep(run[,2],run[,1])
  
  
  nonwear = nonwear+check #sum original nonwear detection plus the wear periods that were checked for non-wear
  nonwear[nonwear>=1]<-1 #any nonwear >=1 is nonwear and make all values 1 for binary outcome; 0/1





