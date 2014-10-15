bowler_record = function(bowler){
  Bowler = data.frame(matrix(nrow=1,ncol=18));
  names(Bowler) = c("Bowler","Country","Matches","Wickets","Runs","Overs","Average","Economy","Strike.Rate","Dot.Ball.Percent","Position1","Freq1","Position2","Freq2","Wicket.Kind1","Wicket.Freq1","Wicket.Kind2","Wicket.Freq2");
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
  if(Bowler$Runs==0){
    Bowler$Average = 0;
  }
  else{
  Bowler$Average = round(Bowler$Runs/Bowler$Wickets,2);
  if(is.infinite(Bowler$Average)){
    Bowler$Average = Bowler$Runs;
  }
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
  over = over[order(over$overs,decreasing=TRUE),];
  Bowler$Position1 = as.vector(over[1,1]);
  Bowler$Freq1 = as.vector(over[1,2]);
  Bowler$Position2 = as.vector(over[2,1]);
  Bowler$Freq2 = as.vector(over[2,2]);
  Outs = Cricket$Wicket.Kind[((Cricket$Dismissed.Player!="")&(Cricket$Bowler==bowler))];
  if(length(Outs)){
  Outs = as.data.frame(table(Outs));
  Outs = Outs[order(Outs$Freq,decreasing=TRUE),];
  Bowler$Wicket.Kind1 = as.vector(Outs[1,1]);
  Bowler$Wicket.Freq1 = as.vector(Outs[1,2]);
  Bowler$Wicket.Kind2 = as.vector(Outs[2,1]);
  Bowler$Wicket.Freq2 = as.vector(Outs[2,2]);}
  return(Bowler);
}