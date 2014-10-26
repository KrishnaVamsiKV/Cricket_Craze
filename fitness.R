fitness_bat = function(Player){
  firsttime = as.numeric(Cricket$Date.No[((Cricket$Batsman==Player)|(Cricket$Bowler==Player))][1]);
  country = as.character(Batsmen_records$Country[(Batsmen_records$Batsman==Player)][1]);
  matchesbycountry = length(unique(Cricket$Match.No[((Cricket$Batting.Team==country)&(as.numeric(Cricket$Date.No)>=firsttime))]));
  matchesbyhim = length(unique(Cricket$Match.No[(Cricket$Batsman==Player)]));
  percent = round(matchesbyhim*100/matchesbycountry,2);
  return(percent);
}

fitness_bowl = function(Player){
  firsttime = as.numeric(Cricket$Date.No[((Cricket$Batsman==Player)|(Cricket$Bowler==Player))][1]);
  country = as.character(Batsmen_records$Country[(Batsmen_records$Batsman==Player)][1]);
  matchesbycountry = length(unique(Cricket$Match.No[((Cricket$Batting.Team==country)&(as.numeric(Cricket$Date.No)>=firsttime))]));
  matchesbyhim = length(unique(Cricket$Match.No[(Cricket$Bowler==Player)]));
  percent = round(matchesbyhim*100/matchesbycountry,2);
  return(percent);
}
