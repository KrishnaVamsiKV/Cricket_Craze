Match_records$points=0
Match_records[(Match_records$Batting.Team==Match_records$Win),28]=1
Match_records$loss=0
for(i in 1:654){
  if((Match_records[i,9]==Match_records[i,27])){
    Match_records[i,29]=Match_records[i,10];
  }
  if((Match_records[i,10]==Match_records[i,27])){
    Match_records[i,29]=Match_records[i,9];
  }
}
Match_records[(Match_records$Batting.Team==Match_records$Win)&(     Match_records$loss==("Australia")|
                                                                      Match_records$loss==("India")|
                                                                      Match_records$loss==("Pakistan")|
                                                                      Match_records$loss==("South Africa")|
                                                                      Match_records$loss==("Sri Lanka")|
                                                                      Match_records$loss==("England")|
                                                                      Match_records$loss==("New Zealand")|
                                                                      Match_records$loss==("West Indies")|
                                                                      Match_records$loss==("Bangladesh")|
                                                                      Match_records$loss==("Zimbabwe")      ),28]=2;








listofmatch=split(Match_records,Match_records$Match.No)
fun=function(record){
  if(nrow(record)==2){
    if(record[1,18]>record[2,18]){
      give=record[1,]
      return(give)
    }
    else return(record[2,])
    
  }
     
}