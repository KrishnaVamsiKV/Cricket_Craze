get_win = function(match){
  Match = data.frame(matrix(nrow=1,ncol=9));
  names(Match) = c("Match.No","Result","Batting1.Runs","Batting2.Runs","Batting1.Run.Rate","Batting2.Run.Rate",
                   "Batting1.Wickets","Batting2.Wickets","Date.No");
  matchdata = Match_records[(Match_records$Match.No==match),];
  if(nrow(matchdata)==2){
  Match$Match.No = match;
  Match$Result = as.numeric(matchdata$Batting.Team[(matchdata$Innings==1)]==
                              matchdata$Win[(matchdata$Innings==1)]);
  Match$Batting1.Runs = matchdata$Total.Runs[(matchdata$Innings==1)];
  Match$Batting2.Runs = matchdata$Total.Runs[(matchdata$Innings==2)];
  Match$Batting1.Run.Rate = matchdata$Run.Rate[(matchdata$Innings==1)];
  Match$Batting2.Run.Rate = matchdata$Run.Rate[(matchdata$Innings==2)];
  Match$Batting1.Wickets = matchdata$Wicket.Kind.Total[(matchdata$Innings==1)];
  Match$Batting2.Wickets = matchdata$Wicket.Kind.Total[(matchdata$Innings==2)];
  Match$Date.No = matchdata$Date.No[1];
  
  }
  return(Match);
}