Cricket_Factors=split(Cricket,Cricket$Match.No+Cricket$Innings/10);
give_df=function(df){
  final_df=data.frame(matrix(nrow=1));
  final_df$Match.No=unique(df$Match.No);
  final_df$Innings=unique(df$Innings);
  final_df$Batting.Team=unique(df$Batting.Team);
  final_df$City=unique(df$City);
  final_df$Date=unique(df$Date);
  final_df$Toss.Decision=unique(df$Toss.Decision);
  final_df$Toss.Winner=unique(df$Toss.Winner);
  final_df$Venue=unique(df$Venue);
  final_df$Team1=unique(df$Team1);
  final_df$Team2=unique(df$Team2);
  final_df$Man.Of.The.Match=unique(df$Man.Of.The.Match);
  final_df$Over=df$Over[nrow(df)];
  final_df$Runs.Batsman=sum(df$Runs.Batsman);
  final_df$Runs.Extras.Wides=sum(df$Runs.Extras.Wides);
  final_df$Runs.Extras.No.Ball=sum(df$Runs.Extras.No.Ball);
  final_df$Runs.Extras.Byes=sum(df$Runs.Extras.Byes);
  final_df$Runs.Extras.Legbyes=sum(df$Runs.Extras.Legbyes);
  final_df$Runs.Extras=sum(df$Runs.Extras.Byes)+sum(df$Runs.Extras.Legbyes)+sum(df$Runs.Extras.No.Ball)+sum(df$Runs.Extras.Wides);
  final_df$Total.Runs=sum(df$Total.Runs);
  aa=as.data.frame(table(df$Wicket.Kind));
  final_df$Wicket.Kind.Caught=sum(if(!(is.na(match("caught",aa[,1])))){aa[aa[,1]=="caught",2]});
  final_df$Wicket.Kind.Caught.And.Bowled=sum(if(!(is.na(match("caught and bowled",aa[,1])))){aa[aa[,1]=="caught and bowled",2]});
  final_df$Wicket.Kind.Bowled=sum(if(!(is.na(match("bowled",aa[,1])))){aa[aa[,1]=="bowled",2]});
  final_df$Wicket.Kind.Lbw=sum(if(!(is.na(match("lbw",aa[,1])))){aa[aa[,1]=="lbw",2]});
  final_df$Wicket.Kind.Runout=sum(if(!(is.na(match("run out",aa[,1])))){aa[aa[,1]=="run out",2]});
  final_df$Wicket.Kind.Stumped=sum(if(!(is.na(match("stumped",aa[,1])))){aa[aa[,1]=="stumped",2]});
  final_df$Wicket.Kind.Total=sum(if(!(is.na(match("caught",aa[,1])))){aa[aa[,1]=="caught",2]},
                                  if(!(is.na(match("bowled",aa[,1])))){aa[aa[,1]=="bowled",2]},
                                  if(!(is.na(match("hit wicket",aa[,1])))){aa[aa[,1]=="hit wicket",2]},
                                  if(!(is.na(match("lbw",aa[,1])))){aa[aa[,1]=="lbw",2]},
                                  if(!(is.na(match("obstructing the field",aa[,1])))){aa[aa[,1]=="obstructing the field",2]},
                                  if(!(is.na(match("retired hurt",aa[,1])))){aa[aa[,1]=="retired hurt",2]},
                                  if(!(is.na(match("run out",aa[,1])))){aa[aa[,1]=="run out",2]},
                                  if(!(is.na(match("stumped",aa[,1])))){aa[aa[,1]=="stumped",2]},
                                  if(!(is.na(match("caught and bowled",aa[,1])))){aa[aa[,1]=="caught and bowled",2]});
  return(final_df);
}
Match_info=lapply(Cricket_Factors,give_df);

Match_info_df=(Match_info[[1]]);
for(i in 2:654){
  Match_info_df=rbind(Match_info_df,Match_info[[i]]);
}



