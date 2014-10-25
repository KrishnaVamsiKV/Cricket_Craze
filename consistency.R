consistency = function(Batsman){
  batsmandata = Cricket[(Cricket$Batsman==Batsman),];
  matches = unique(batsmandata$Match.No);
  Batsman_rec = data.frame(matrix(nrow=1,ncol=6));
  names(Batsman_rec) = c("Batsman","Country","Strike.Rate.Mean","Average.Mean"
                         ,"Strike.Rate.Sd","Average.Sd");
  Batsman_rec$Batsman = Batsman;
  Batsman_rec$Country = Cricket$Batting.Team[(Cricket$Batsman==Batsman)][1];
  len = length(matches);
  last = len;
  Strike.Rate = c();
  Average = c();
  for(i in 1:(last-9)){
    match = matches[(last-9):last];
    last = last - 1;
    runs = 0; balls = 0;outs = 0;
    for(j in match){
      batsmandata = Cricket[((Cricket$Batsman==Batsman)&(Cricket$Match.No==j)),];
      runs = runs + sum(batsmandata$Runs.Batsman);
      balls = balls + length(batsmandata$Ball.No[(batsmandata$Runs.Extras.Wides==0)]);
      outs = outs + length(Cricket$Ball.No[((Cricket$Dismissed.Player==Batsman)&(Cricket$Match.No==j))]);
    }
    if(runs==0){
      avg = 0;
    }
    else{
      avg = round(runs/outs,2);
      if(is.infinite(avg)){
        avg = runs;
      }
    }
    Strike.Rate = c(Strike.Rate,round(runs*100/balls,2));
    Average = c(Average,avg);
  }
  Batsman_rec$Average.Mean = round(mean(Average),2);
  Batsman_rec$Strike.Rate.Mean = round(mean(Strike.Rate),2);
  Batsman_rec$Average.Sd = round(sd(Average),2);
  Batsman_rec$Strike.Rate.Sd = round(sd(Strike.Rate),2);
  return(Batsman_rec);
}

bowler_consistency = function(bowler){
  bowlerdata = Cricket[(Cricket$Bowler== bowler),];
  matches = unique(bowlerdata$Match.No);
  Bowler = data.frame(matrix(nrow=1,ncol=8));
  names(Bowler) = c("Bowler","Country","Economy.Mean","Average.Mean","Dot.Percent.Mean"
                         ,"Economy.Sd","Average.Sd","Dot.Percent.Sd");
  Bowler$Bowler = bowler;
  record = Cricket[(Cricket$Bowler==bowler),];
  if(record[1,]$Batting.Team==record[1,]$Team1){
    Bowler$Country = record[1,]$Team2;
  }
  else{
    Bowler$Country = record[1,]$Team1;
  }
  len = length(matches);
  last = len;
  Economy = c();
  Average = c();
  Dot.Percent = c();
  for(i in 1:(last-9)){
    match = matches[(last-9):last];
    last = last - 1;
    runs = 0; balls = 0;outs = 0;dots = 0;
    for(j in match){
      bowlerdata = Cricket[((Cricket$Bowler== bowler)&(Cricket$Match.No==j)),];
      runs = runs + sum(bowlerdata$Runs.Batsman)+sum(bowlerdata$Runs.Extras.No.Ball)+sum(bowlerdata$Runs.Extras.Wides);
      balls = balls + length(bowlerdata$Ball.No[((bowlerdata$Runs.Extras.Wides==0)&(bowlerdata$Runs.Extras.No.Ball==0))]);
      outs = outs + length(bowlerdata$Ball.No[(bowlerdata$Dismissed.Player!="")]);
      dots = dots + length(bowlerdata$Ball.No[(bowlerdata$Total.Runs==0)]);
    }
    if(runs==0){
      avg = 0;
    }
    else{
      avg = round(runs/outs,2);
      if(is.infinite(avg)){
        avg = runs;
      }
    }
    Economy = c(Economy,round(runs*6/balls,2));
    Average = c(Average,avg);
    Dot.Percent = c(Dot.Percent,round(dots*100/balls,2));
  }
  Bowler$Average.Mean = round(mean(Average),2);
  Bowler$Economy.Mean = round(mean(Economy),2);
  Bowler$Average.Sd = round(sd(Average),2);
  Bowler$Economy.Sd = round(sd(Economy),2);
  Bowler$Dot.Percent.Mean = round(mean(Dot.Percent),2);
  Bowler$Dot.Percent.Sd = round(sd(Dot.Percent),2);
  return(Bowler);
}
