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

