Cricket = read.csv("Cricket.csv",stringsAsFactors = F);
names(Cricket)[1] = "Ball.No";
names(Cricket)[2] = "Match.No";
names(Cricket)[4] = "Batting.Team";
names(Cricket)[19] = "Runs.Extras.Wides";
names(Cricket)[20] = "Runs.Extras.No.Ball";
names(Cricket)[21] = "Runs.Extras.Byes";
names(Cricket)[22] = "Runs.Extras.Legbyes";
Teams = unique(Cricket$Batting.Team);
Matches = unique(Cricket$Match.No);
Cities = unique(Cricket$City);
Venues = unique(Cricket$Venue);
Batsmen = unique(Cricket$Batsman)
Bowlers = unique(Cricket$Bowler)
Fielders = unique(Cricket$Wicket.Fielder)
Players = c(Batsmen,Bowlers,Fielders);
Players = unique(Players);
Wicket.Kinds = unique(Cricket$Wicket.Kind);

batsmen_record = function(batsman){
  Batsman = data.frame(matrix(nrow=1,ncol=12));
  names(Batsman) = c("Batsman","Country","Matches","Runs","Average","Strike.Rate","Outs","Balls","Fours","Sixes","Position1","Position2");
  Batsman$Batsman = batsman;
  Batsman$Country = Cricket$Batting.Team[(Cricket$Batsman==batsman)][1];
  Batsman$Matches = length(unique(Cricket$Match.No[(Cricket$Batsman==batsman)]));
  Batsman$Runs = sum(Cricket$Runs.Batsman[(Cricket$Batsman==batsman)]);
  Batsman$Balls = length(Cricket$Ball.No[((Cricket$Batsman==batsman)&(Cricket$Runs.Extras.Wides==0))]);
  Batsman$Outs = length(Cricket$Ball.No[(Cricket$Dismissed.Player==batsman)]);
  Batsman$Average = round(Batsman$Runs/Batsman$Outs,2);
  if(is.infinite(Batsman$Average)){
    Batsman$Average = Batsman$Runs;
  }
  Batsman$Strike.Rate = round(Batsman$Runs*100/Batsman$Balls,2);
  Batsman$Fours = sum(Cricket$Runs.Batsman[(Cricket$Batsman==batsman)]==4);
  Batsman$Sixes = sum(Cricket$Runs.Batsman[(Cricket$Batsman==batsman)]==6);
  Batsman$Position1 = as.vector(get_position(Batsman$Batsman)[1]);
  Batsman$Position2 = as.vector(get_position(Batsman$Batsman)[2]);
  return(Batsman);
}

Batsmen_records =  as.data.frame(t(sapply(Batsmen,batsmen_record)),row.names=FALSE);

bowler_record = function(bowler){
  Bowler = data.frame(matrix(nrow=1,ncol=12));
  names(Bowler) = c("Bowler","Country","Matches","Wickets","Runs","Overs","Average","Economy","Strike.Rate","Dot.Ball.Percent","Position1","Position2");
  Bowler$Bowler = bowler;
  record = Cricket[(Cricket$Bowler==bowler),];
  if(record[1,]$Batting.Team==record[1,]$Team1){
    Bowler$Country = record[1,]$Team2;
  }
  else{
    Bowler$Country = record[1,]$Team1;
  }
  Bowler$Matches = length(unique(Cricket$Match.No[(Cricket$Bowler==bowler)]));
  Bowler$Runs = sum(Cricket$Runs.Batsman[(Cricket$Bowler==bowler)])+sum(Cricket$Runs.Extras.No.Ball[(Cricket$Bowler==bowler)])+sum(Cricket$Runs.Extras.Wides[(Cricket$Bowler==bowler)]);
  Balls = length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Runs.Extras.Wides==0)&(Cricket$Runs.Extras.No.Ball==0))]);
  Bowler$Overs = round(Balls/6,0);
  Bowler$Wickets = length(Cricket$Ball.No[((Cricket$Dismissed.Player!="")&(Cricket$Bowler==bowler))]);
  Bowler$Average = round(Bowler$Runs/Bowler$Wickets,2);
  if(is.infinite(Bowler$Average)){
    Bowler$Average = Bowler$Runs;
  }
  Bowler$Economy = round(Bowler$Runs*6/Balls,2);
  Bowler$Strike.Rate = round(Balls/Bowler$Wickets,2);
  if(is.infinite(Bowler$Strike.Rate)){
    Bowler$Strike.Rate = Balls;
  }
  Bowler$Dot.Ball.Percent = round(length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Total.Runs==0))])*100/Balls,2);
  overs = length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Over<=10))]);
  overs = c(overs,length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Over>10)&(Cricket$Over<=20))]));
  overs = c(overs,length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Over>20)&(Cricket$Over<=40))]));
  overs = c(overs,length(Cricket$Ball.No[((Cricket$Bowler==bowler)&(Cricket$Over>40)&(Cricket$Over<=50))]));
  over = data.frame(cbind(1:4),overs);
  over = over[order(over$overs,decreasing=TRUE),1];
  Bowler$Position1 = as.vector(over)[1];
  Bowler$Position2 = as.vector(over)[2];
  return(Bowler);
}

Bowlers_records =  as.data.frame(t(sapply(Bowlers,bowler_record)),row.names=FALSE);

get_position = function(batsman){
  matches = unique(Cricket$Match.No[(Cricket$Batsman==batsman)]);
  positions = c();
  for(match in matches){
    positions = c(positions,get_pos(match,batsman));
  }
  positions = as.data.frame(table(positions));
  position = positions[order(positions$Freq,decreasing=TRUE),1];
  return(position);
}

get_pos = function(match,batsman){
  country = Cricket$Batting.Team[(Cricket$Batsman==batsman)][1];
  batting = unique(Cricket$Batsman[((Cricket$Match.No==match)&(Cricket$Batting.Team==country))]);
  position = which(batting == batsman);
  return(position);
}
