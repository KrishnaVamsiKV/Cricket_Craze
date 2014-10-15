get_partnership=function(ab){
  ab$Unique=paste(ab$Match.No,ab$Batsman,ab$NonStriker,sep=" ");
  required=split(ab,paste(ab$Match.No,ab$Batsman,ab$NonStriker,sep=" "))
  list_partner=lapply(required,get_partnership_1)
  Partnership_info_df=(list_partner[[1]]);
  for(i in 2:9796){
    Partnership_info_df=rbind(Partnership_info_df,list_partner[[i]]);
  }
  Partnership_info_df$player2_score=apply(Partnership_info_df[,c(3,10,11)],1,get_result)
  Part = Partnership_info_df;
  apply(Part[,c(3,10,11)],1,removef);
  Partnership_info_df;
}
  get_partnership_1=function(df){
    partner=data.frame(matrix(nrow=1))
    partner$Ball.No=nrow(df)
    partner$Match.No=unique(df$Match.No)
    partner$Innings=unique(df$Innings)
    partner$Batting.Team=unique(df$Batting.Team)
    partner$City=unique(df$City)
    partner$Date=unique(df$Date)
    partner$Venue=unique(df$Venue)
    partner$Man.of.the.Match=unique(df$Man.of.the.Match)
    partner$Batsman=unique(df$Batsman)
    partner$NonStriker=unique(df$NonStriker)
    partner$Runs.Batsman=sum(df$Runs.Batsman)
    return(partner)
  }

get_result=function(r_1){
  ret=Partnership_info_df[((r_1[1]==Partnership_info_df$Match.No)
                           &(r_1[2]==Partnership_info_df$NonStriker)
                           &(r_1[3]==Partnership_info_df$Batsman)),12];
  if(length(ret)==0){
    ret = 0;
  }
  return(ret);
  
}
removef=function(r_1){
  Partnership_info_df = Partnership_info_df[!((r_1[1]==Partnership_info_df$Match.No)
                                              &(r_1[2]==Partnership_info_df$NonStriker)
                                              &(r_1[3]==Partnership_info_df$Batsman)),];
  
}

