recent_batsman = function(Batsman){
  batsmandata = Cricket[(Cricket$Batsman==Batsman),];
  batsmandata = batsmandata[(batsmandata$Date.No>731),];
  matches = unique(batsmandata$Match.No);
  Batsman_rec = data.frame(matrix(nrow=1,ncol=4));
  names(Batsman_rec) = c("Batsman","Country","Recent.Strike.Rate","Recent.Average");
  Batsman_rec$Batsman = Batsman;
  Batsman_rec$Country = Cricket$Batting.Team[(Cricket$Batsman==Batsman)][1];
  runs = sum(batsmandata$Runs.Batsman);
  balls =  length(batsmandata$Ball.No[(batsmandata$Runs.Extras.Wides==0)]);
  outs =  length(batsmandata$Ball.No[(batsmandata$Dismissed.Player==Batsman)]);
  if(runs==0){
      avg = 0;
  }
  else{
    avg = round(runs/outs,2);
    if(is.infinite(avg)){
      avg = runs;
    }
  }
    Batsman_rec$Recent.Strike.Rate = round(runs*100/balls,2);
    Batsman_rec$Recent.Average = avg;
    return(Batsman_rec);
}

recent_bowler = function(bowler){
  bowlerdata = Cricket[(Cricket$Bowler== bowler),];
  bowlerdata = bowlerdata[(bowlerdata$Date.No>731),];
  matches = unique(bowlerdata$Match.No);
  Bowler = data.frame(matrix(nrow=1,ncol=5));
  names(Bowler) = c("Bowler","Country","Recent.Average","Recent.Economy","Recent.Dot.Percent" );
  Bowler$Bowler = bowler;
  record = Cricket[(Cricket$Bowler==bowler),];
  if(record[1,]$Batting.Team==record[1,]$Team1){
    Bowler$Country = record[1,]$Team2;
  }
  else{
    Bowler$Country = record[1,]$Team1;
  }
      runs =  sum(bowlerdata$Runs.Batsman)+sum(bowlerdata$Runs.Extras.No.Ball)+sum(bowlerdata$Runs.Extras.Wides);
      balls =  length(bowlerdata$Ball.No[((bowlerdata$Runs.Extras.Wides==0)&(bowlerdata$Runs.Extras.No.Ball==0))]);
      outs =  length(bowlerdata$Ball.No[(bowlerdata$Dismissed.Player!="")]);
      dots =  length(bowlerdata$Ball.No[(bowlerdata$Total.Runs==0)]);
    
    if(runs==0){
      avg = 0;
    }
    else{
      avg = round(runs/outs,2);
      if(is.infinite(avg)){
        avg = runs;
      }
    }
    Bowler$Recent.Economy = round(runs*6/balls,2);
    Bowler$Recent.Average = avg;
    Bowler$Recent.Dot.Percent = round(dots*100/balls,2);
  
  return(Bowler);
}