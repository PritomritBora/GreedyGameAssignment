setwd("/users/HP/desktop/Greddy")

text <- readLines("datadump.txt") 

id <- regexpr('\\b[0-9]{8}\\b',text)
 game_id <- regmatches(text,id)                        

s <- regexpr('\\d\\.\\d',text)    
sdkv <- regmatches(text,s)                                          

r <- regexpr("gg[a-z]*",text)
event <- regmatches(text,r)                           

 t <- regexpr("\\b[0-9]{6,13}\\b",text)
 ts <- regmatches(text,t)                                                    

stamp <- regexpr('\\b\\d+-\\d+-\\d+\\s\\d+:\\d+:\\d+\\.\\d+\\b',text)
 timestamp <- regmatches(text,stamp) 
                                              
i5 <- regexpr('\\b[a-z0-9]{32}\\b',text)
ai5 <- regmatches(text,i5)   
greedy<-data.frame(ai5,game_id,event,ts,timestamp,sdkv)                                                           
cols.num <- c("event","ai5")
greedy[cols.num] <- sapply(greedy[cols.num],as.character)
cols.num <- c("game_id","sdkv")
greedy[cols.num] <- sapply(greedy[cols.num],as.character)
greedy[cols.num] <- sapply(greedy[cols.num],as.numeric)
cols.num <- c("ai5","event")
greedy[cols.num] <- sapply(greedy[cols.num],as.character)
greedy$ts<-as.character(as.factor(greedy$ts))
greedy$ts<-as.numeric(as.character(greedy$ts))
options(digits.secs = 3)
greedy$ts<-as.POSIXct(greedy$ts/1000, origin="1970-01-01")
greedy$timestamp<-as.character(as.factor(greedy$timestamp))
greedy$timestamp<-as.POSIXct(greedy$timestamp)


 test<-greedy
 length(test$ts)
 test$timestamp<-NULL
 test<-unique(test)
 test<- test[order(test[1],test[4]),]
 test_start<- test[test$event=="ggstart",]
 test_stop<- test[test$event=="ggstop",]
 library(plyr)
 require(dplyr)
 df1<-anti_join(test_start,test_stop,by=c("ai5"))
 df2<-anti_join(test_stop,test_start,by=c("ai5"))
 test<-test[!(test$ai5 %in% df1$ai5),]
 test<-test[!(test$ai5 %in% df2$ai5),]
 test$lid<-seq.int(nrow(test))
 test1<-test %>% filter(ai5 != lead(ai5) | event != lead(event) | row_number() == 1)
 
 CR<-anti_join(test,test1,by=c("lid"))
 CR_start<-CR[CR$event=="ggstart",]
 CR_stop<-CR[CR$event=="ggstop",]
 test<-test %>% filter(ai5 != lead(ai5) | event != lead(event) | row_number() == 1)
 test$lid<-seq.int(nrow(test))
 edge<-ddply(test,.(ai5),function(x) head(x,1))
 edge_e<-ddply(test,.(ai5),function(x) tail(x,1))
 edge_start<-edge_e[edge_e$event=="ggstart",]
 edge_stop<-edge[edge$event=="ggstop",]
 test<-test[!(test$lid %in% edge_start$lid),]
 test<-test[!(test$lid %in% edge_stop$lid),]
 edge_start$lid<-NULL
 CR_start$lid<-NULL
 df1$lid<-NULL
 edge_stop$lid<-NULL
 CR_stop$lid<-NULL
 df2$lid<-NULL
 test_start<-rbind(CR_start,df1)
 test_start<-rbind(test_start,edge_start)
 test_start<- test_start[order(test_start$ai5,test_start$ts),]
 test_stop<-rbind(CR_stop,df2)
 test_stop<-rbind(test_stop,edge_stop)
 test_stop<- test_stop[order(test_stop$ai5,test_stop$ts),]
 test_start<-test_start%>%
   group_by(ai5)%>%
   mutate(tdiff = c(0,diff(ts)))
 test_stop<-test_stop%>%
   group_by(ai5)%>%
   mutate(tdiff = c(0,diff(ts)))
 test<-test%>%
  group_by(ai5)%>%
  mutate(tdiff = c(0,diff(ts)))
length(test$tdiff)

valid_count<-function(x) {
  i<-2
  j<-0
  counter<-0
  while(i<=length(x$tdiff)){
    j<-i+1
    sum<-x[i,7]
    while((x[j,7]<30) && (j<length(x$tdiff))){
      
      sum<-sum+x[j+1,7]
      j<-j+2
      
    }
    ifelse((sum>60),counter<-counter+1,counter<-counter)
    i<-j+1
    
  }
  return(counter)
}

Session_count<-function(x) {
  i<-2
  sum<-x[i,7]
  counter<-0

  while(i<=length(x$tdiff)){
    j<-i+1
    while((x[j,7]<30) && (j<length(x$tdiff))){
      sum<-sum+x[j+1,7]
      j<-j+2
    }
    counter<-counter+1
    i<-j+1
  }
  return(counter)
}
valid_sessionlength<-function(x) {
  i<-2
  total_sum<-0
  counter<-0
  while(i<=length(x$tdiff)){
     j<-i+1
    sum<-x[i,7]
    while((x[j,7]<30) && (j<length(x$tdiff)+1)){
      
      sum<-sum+x[j+1,7]
      j<-j+2
      
    }
    ifelse((sum>60),total_sum<-total_sum+sum,total_sum<-total_sum)
    i<-j+1
  }
  return(total_sum)
}
session_count_loss<-function(x){
  total_sum<-0
   counter<-0
   i<-1
   
while(i<length(x$tdiff)){
   j<-i+1
  
  while((x[j,6]<30) && (j<length(x$tdiff))){
    
    j<-j+1
    
  }
   
  counter<-counter+1
  i<-j+1
}
return(counter)
}
  

valid_session_count<-ddply(test,.(ai5),function(x){valid_count(x)})
session_count<-ddply(test,.(ai5),function(x){Session_count(x)})
Valid_session_length<-ddply(test,.(ai5),function(x){valid_sessionlength(x)})
session_lost_data<-ddply(test_start,.(ai5),function(x){session_count_loss(x)})
session_lost_data1<-ddply(test_stop,.(ai5),function(x){session_count_loss(x)})
Total_session<-sum(session_count$V1)
Total_valid_session<-sum(valid_session_count$V1)
Total_valid_length<-sum(Valid_session_length$V1)
avg_valid_time<-Total_valid_length/Total_valid_session
session_lostdata<-rbind(session_lost_data,session_lost_data1)
probable_sessions_from_lostdata<-sum(session_lostdata$V1)
Total_session
Total_valid_session
avg_valid_time
probable_sessions_from_lostdata

