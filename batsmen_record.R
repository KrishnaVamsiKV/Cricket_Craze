# Function to generate a batsman record #
batsmen_record = function(batsman){
  Batsman = data.frame(matrix(nrow=1,ncol=18));
  names(Batsman) = c("Batsman","Country","Matches","Runs","Average","Strike.Rate","Outs","Balls","Fours","Sixes","Position1","Freq1","Position2","Freq2","Wicket.Kind1","Wicket.Freq1","Wicket.Kind2","Wicket.Freq2");
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
  Batsman$Position1 = as.vector(get_position(Batsman$Batsman)[1,1]);
  Batsman$Freq1 = as.vector(get_position(Batsman$Batsman)[1,2]);
  Batsman$Position2 = as.vector(get_position(Batsman$Batsman)[2,1]);
  Batsman$Freq2 = as.vector(get_position(Batsman$Batsman)[2,2]);
  Outs = Cricket$Wicket.Kind[(Cricket$Dismissed.Player==batsman)];
  if(length(Outs)){
  Outs = as.data.frame(table(Outs));
  Outs = Outs[order(Outs$Freq,decreasing=TRUE),];
  Batsman$Wicket.Kind1 = as.vector(Outs[1,1]);
  Batsman$Wicket.Freq1 = as.vector(Outs[1,2]);
  Batsman$Wicket.Kind2 = as.vector(Outs[2,1]);
  Batsman$Wicket.Freq2 = as.vector(Outs[2,2]);}
  return(Batsman);
}

get_position = function(batsman){
  matches = unique(Cricket$Match.No[(Cricket$Batsman==batsman)]);
  positions = c();
  for(match in matches){
    positions = c(positions,get_pos(match,batsman));
  }
  positions = as.data.frame(table(positions));
  position = positions[order(positions$Freq,decreasing=TRUE),];
  return(position);
}

get_pos = function(match,batsman){
  country = Cricket$Batting.Team[(Cricket$Batsman==batsman)][1];
  batting = unique(Cricket$Batsman[((Cricket$Match.No==match)&(Cricket$Batting.Team==country))]);
  position = which(batting == batsman);
  return(position);
}




