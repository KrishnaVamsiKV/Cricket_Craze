analysis = function(){
  # Generating number of good players in a team for each factor #
  k = 24;
  for(j in c(4,5,6,24,25,26,27,28,29)){
    batsmen = TopBatsmen[order(as.numeric(TopBatsmen[,j]),decreasing=TRUE),][1:25,];
    country = as.character(TopTeams$Country);
    x = c(1:10);
    for(i in 1:10){
      x[i] = nrow(batsmen[(batsmen$Country==country[i]),]);
    }
    TopTeams = cbind(TopTeams,x);
    names(TopTeams)[k] = names(TopBatsmen)[j];
    k = k + 1;
  }
  
  for(j in c(21,22)){
    batsmen = TopBatsmen[order(as.numeric(TopBatsmen[,j]),decreasing=FALSE),][1:25,];
    country = as.character(TopTeams$Country);
    x = c(1:10);
    for(i in 1:10){
      x[i] = nrow(batsmen[(batsmen$Country==country[i]),]);
    }
    TopTeams = cbind(TopTeams,x);
    names(TopTeams)[k] = names(TopBatsmen)[j];
    k = k + 1;
  }
  
  for(j in c(7,8,9,22,23,24,29,30)){
    bowlers = TopBowlers[order(as.numeric(TopBowlers[,j]),decreasing=FALSE),][1:25,];
    country = as.character(TopTeams$Country);
    x = c(1:10);
    for(i in 1:10){
      x[i] = nrow(bowlers[(bowlers$Country==country[i]),]);
    }
    TopTeams = cbind(TopTeams,x);
    names(TopTeams)[k] = paste("B" ,names(TopBowlers)[j]);
    k = k + 1;
  }
  for(j in c(4,10,26,27,28,31,32,33)){
    bowlers = TopBowlers[order(as.numeric(TopBowlers[,j]),decreasing=TRUE),][1:25,];
    country = as.character(TopTeams$Country);
    x = c(1:10);
    for(i in 1:10){
      x[i] = nrow(bowlers[(bowlers$Country==country[i]),]);
    }
    TopTeams = cbind(TopTeams,x);
    names(TopTeams)[k] = paste("B" ,names(TopBowlers)[j]);
    k = k + 1;
  }
  
  # Generating Points for each Team #
  TopTeams$Points = 0;
  countries = as.character(TopTeams$Country);
  for(i in 1:10){
    TopTeams$Points[i] = sum(Match_records$points[(Match_records$Win==countries[i])])/(as.numeric(TopTeams$Matches[i]));
  }
  TopTeams = TopTeams[order(TopTeams$Points,decreasing=TRUE),];
  
  # Seeing MI for different factors #
  x = 1:10;
  info = 24:50;
  for(i in 1:27){
    info[i] = mutinformation(x,as.numeric(TopTeams[,(i+23)]));
  }
  info = as.data.frame(cbind(info,c(24:50)));
  info = info[order(info$info,decreasing=TRUE),];
  factor = info[,2];
  info = cbind(names(TopTeams)[factor],info);
  
  # Seeing MI for different factors for batsmen in different positions #
  factor_bat = c();
  for(p in 1:8){
    TopTeams = TopTeams[,c(1:23)];
    k = 24;
    for(j in c(5,6,24,25,26,27,28)){
      batsmen = TopBatsmen[((TopBatsmen$Position1==p)|(TopBatsmen$Position2==p)),]
      batsmen = batsmen[order(as.numeric(batsmen[,j]),decreasing=TRUE),][1:10,];
      country = as.character(TopTeams$Country);
      x = c(1:10);
      for(i in 1:10){
        x[i] = nrow(batsmen[(batsmen$Country==country[i]),]);
      }
      TopTeams = cbind(TopTeams,x);
      names(TopTeams)[k] = names(TopBatsmen)[j];
      k = k + 1;
    }
    
    
    x = 1:10;
    info_bat = 24:30;
    for(i in 1:7){
      info_bat[i] = mutinformation(x,as.numeric(TopTeams[,(i+23)]));
    }
    info_bat = as.data.frame(cbind(info_bat,c(24:30)));
    info_bat = info_bat[order(info_bat$info_bat,decreasing=TRUE),];
    factorb = info_bat[,2];
    info_bat = cbind(names(TopTeams)[factorb],info_bat);
    factor_bat = as.data.frame(cbind(factor_bat,info_bat[,1]));
    if(p==1){
      factor_bat = as.data.frame(cbind(factor_bat,info_bat[,1]));
    }
  }
  
  # Seeing MI for different factors for bowlers #
  factor_bowl = c();
  for(p in 1:4){
    TopTeams = TopTeams[,c(1:23)]
    k = 24;
    for(j in c(7,8,9,29,30)){
      bowlers = TopBowlers[(TopBowlers$Position1==p),];
      bowlers = bowlers[order(as.numeric(bowlers[,j]),decreasing=FALSE),][1:10,];
      country = as.character(TopTeams$Country);
      x = c(1:10);
      for(i in 1:10){
        x[i] = nrow(bowlers[(bowlers$Country==country[i]),]);
      }
      TopTeams = cbind(TopTeams,x);
      names(TopTeams)[k] = paste("B" ,names(TopBowlers)[j]);
      k = k + 1;
    }
    for(j in c(4,10,26,27,28,31,33)){
      bowlers = TopBowlers[(TopBowlers$Position1==p),];
      bowlers = bowlers[order(as.numeric(bowlers[,j]),decreasing=TRUE),][1:10,];
      country = as.character(TopTeams$Country);
      x = c(1:10);
      for(i in 1:10){
        x[i] = nrow(bowlers[(bowlers$Country==country[i]),]);
      }
      TopTeams = cbind(TopTeams,x);
      names(TopTeams)[k] = paste("B" ,names(TopBowlers)[j]);
      k = k + 1;
    }
    
    x = 1:10;
    info_bowl = 24:35;
    for(i in 1:12){
      info_bowl[i] = mutinformation(x,as.numeric(TopTeams[,(i+23)]));
    }
    info_bowl = as.data.frame(cbind(info_bowl,c(24:35)));
    info_bowl = info_bowl[order(info_bowl$info_bowl,decreasing=TRUE),];
    factorbo = info_bowl[,2];
    info_bowl = cbind(names(TopTeams)[factorbo],info_bowl);
    factor_bowl = as.data.frame(cbind(factor_bowl,info_bowl[,1]));
    if(p==1){
      factor_bowl = as.data.frame(cbind(factor_bowl,info_bowl[,1]));
    }
  }
  
  # Generating Top Bowlers #
  crit1 = as.numeric(TopBowlers$Dot.Ball.Percent)/as.numeric(TopBowlers$Economy)*as.numeric(TopBowlers$Rating)/as.numeric(TopBowlers$Recent.Average)
  TopBowlers$Bowler[order(crit1,decreasing=TRUE)][1:5]
  
  crit3 = as.numeric(TopBowlers$Dot.Ball.Percent)/as.numeric(TopBowlers$Economy)*as.numeric(TopBowlers$OpM)*as.numeric(TopBowlers$Wickets)*as.numeric(TopBowlers$Mom)/as.numeric(TopBowlers$Recent.Average)
  TopBowlers$Bowler[order(crit3,decreasing=TRUE)][1:5]
}