

#'The sample data has been converted to 1 sec windows
#'A sleep algorithm was used to provide sleep start and end times and was used to annotate
#'the dataset with sleep predictions; variable "sleep" is annotated with 's' for instances identified as sleep
#'Variable "nonwear" is annotated with 1 for nonwear and 0 for wear
#'


url<-RCurl::getURLContent("https://raw.githubusercontent.com/MA-QUT/Nonwear/master/sample_sleep_data.csv")
sample<-data.table::fread(url) 
hold<-ifelse(sample$sleep%in%"s",1,0)

hold<-c(0,diff(hold))

st<-which(hold==1) #index start of sleep
end<-which(hold==-1) #index end of sleep
end<-end-1
if(length(st)>length(end)){end<-c(end,length(hold))} #In case monitoring period ends during a sleep period

for(nw in 1:length(st)){
  non<-length(which(sample$nonwear[st[nw]:end[nw]]==1)) #determine length of non-wear within sleep duration
  sl<-length(which(sample$sleep[st[nw]:end[nw]]%in%"s")) #determine length of sleep duration
  if(non/sl<.75){sample$nonwear[st[nw]:end[nw]]<-2} #apply 75% rule
  
}
#Variable "nonwear" now has 0 for wear, 1 for nonwear, and 2 for sleep


