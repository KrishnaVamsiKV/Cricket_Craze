Cricket1=Cricket;
Cricket1$Unique=paste(Cricket1$Match.No,Cricket1$Batsman,Cricket1$NonStriker,sep=" ");
required=split(Cricket1,paste(Cricket1$Match.No,Cricket1$Batsman,Cricket1$NonStriker,sep=" "))
get_partnership=function(df){
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
list_partner=lapply(required,get_partnership)
Partnership_info_df=(list_partner[[1]]);
for(i in 2:9796){
  Partnership_info_df=rbind(Partnership_info_df,list_partner[[i]]);
}

