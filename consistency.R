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
  for(i in 1:floor(len/10)){
    match = matches[last-9:last];
    last = last - 10;
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