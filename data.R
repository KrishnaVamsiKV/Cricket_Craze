# Loading the data frame and changing the names #
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
Cricket$Date.No = difftime(strptime(Cricket$Date,format = "%d-%m-%Y"),strptime("01-01-2011","%d-%m-%Y"),units="days");
Cricket = Cricket[order(Cricket$Date.No),];

# Generating batsmen records #
Batsmen_records =  as.data.frame(t(sapply(Batsmen,batsmen_record)),row.names=FALSE);

# Generating bowlers records #
Bowlers_records =  as.data.frame(t(sapply(Bowlers,bowler_record)),row.names=FALSE);

# Generating Partnership information #
Partnership_records=get_partnership(Cricket);

# Generating Match Information#
Match_records = get_matches(Cricket);
Match_records$Date.No = difftime(strptime(Match_records$Date,format = "%d-%m-%Y"),strptime("01-01-2011","%d-%m-%Y"),units="days");
Match_records = Match_records[order(Match_records$Date.No),];

# Generating Teams Performance #
Team_records = as.data.frame(t(sapply(Teams,team_info)),row.names=FALSE);
Team_performance = as.data.frame(t(sapply(Teams,team_performance)),row.names=FALSE);
Team_performance$Win.Percent = round(as.numeric(Team_performance$Won)*100/as.numeric(Team_performance$Matches),2); 
TopTeams = Team_performance[(Team_performance$Matches>15),];
TopTeams = TopTeams[order(TopTeams$Win.Percent,decreasing=TRUE),];
bind = c();
for(i in 1:nrow(TopTeams)){
  bind = rbind(bind,Team_records[(Team_records$Country==as.character(TopTeams$Country[i])),]);
}
TopTeams = cbind(TopTeams,bind[,c(4:20)]);


# Generating Consistence Criteria #
# Batsmen #
Batsmen_records_req = Batsmen_records[(Batsmen_records$Runs>600),];
Batsmen_req = as.character(Batsmen_records_req$Batsman);
Batsmen_Consistency = as.data.frame(t(sapply(Batsmen_req,consistency)),row.names=FALSE);
matches = c(1:74);
Batsmen_req = as.character(Batsmen_Consistency$Batsman);
for(i in 1:74){
matches[i] = Batsmen_records$Matches[(Batsmen_records$Batsman==Batsmen_req[i])];
}
streaks = as.numeric(matches) - 10;
Batsmen_Consistency$Streaks = streaks;
Batsmen_Consistency$Rating = round((as.numeric(Batsmen_Consistency$Average.Mean)*(log((as.numeric(Batsmen_Consistency$Average.Mean)-19)/12)*as.numeric(Batsmen_Consistency$Strike.Rate.Mean))*as.numeric(Batsmen_Consistency$Streaks))
                                     /(as.numeric(Batsmen_Consistency$Average.Sd)),2);
 
Batsmen_Consistency = Batsmen_Consistency[order(Batsmen_Consistency$Rating,decreasing=TRUE),];

# Bowlers #
Bowlers_records_req = Bowlers_records[(Bowlers_records$Wickets>25),];
Bowlers_req = as.character(Bowlers_records_req$Bowler);
Bowlers_Consistency = as.data.frame(t(sapply(Bowlers_req,bowler_consistency)),row.names=FALSE);
matches = c(1:59);
Bowlers_req = as.character(Bowlers_Consistency$Bowler);
for(i in 1:59){
  matches[i] = Bowlers_records$Matches[(Bowlers_records$Bowler==Bowlers_req[i])];
}
streaks = as.numeric(matches) - 10;
Bowlers_Consistency$Streaks = streaks;
Bowlers_Consistency$WpM = round((as.numeric(Bowlers_Consistency$Economy.Mean))*10/(as.numeric(Bowlers_Consistency$Average.Mean)),2);
Bowlers_Consistency$Rating = round(((as.numeric(Bowlers_Consistency$Dot.Percent.Mean))*(as.numeric(Bowlers_Consistency$Streaks)))/
                                     ((as.numeric(Bowlers_Consistency$Average.Mean))*(as.numeric(Bowlers_Consistency$Economy.Mean))*(as.numeric(Bowlers_Consistency$Average.Sd))*(log(as.numeric(Bowlers_Consistency$Average.Mean)/6))),2);
Bowlers_Consistency$WpM = round((as.numeric(Bowlers_Consistency$Economy.Mean))*10/(as.numeric(Bowlers_Consistency$Average.Mean)),2);
Bowlers_Consistency = Bowlers_Consistency[order(Bowlers_Consistency$Rating,decreasing=TRUE),];


bind = c();
for(i in 1:nrow(Batsmen_Consistency)){
  bind = rbind(bind,Batsmen_records[(Batsmen_records$Batsman==as.character(Batsmen_Consistency$Batsman[i])),]);
}
bind2 = Batsmen_Consistency[,c(3,4,5,6,7,8)];
TopBatsmen = cbind(bind,bind2);

bind = c();
for(i in 1:nrow(Bowlers_Consistency)){
  bind = rbind(bind,Bowlers_records[(Bowlers_records$Bowler==as.character(Bowlers_Consistency$Bowler[i])),]);
}
bind2 = Bowlers_Consistency[,c(3,4,5,6,7,8,9,10,11)];
TopBowlers = cbind(bind,bind2);


# Number of 100+ Partnerships #
TopBatsmen$Partnerships = sapply(as.vector(TopBatsmen$Batsman),partnerships);

# Number of Man of the Matches #
TopBatsmen$Mom = sapply(as.vector(TopBatsmen$Batsman),get_mom);
TopBowlers$Mom = sapply(as.vector(TopBowlers$Bowler),get_mom);

# Generating Recent Performances #
bind_bat = as.data.frame(t(sapply(as.character(TopBatsmen$Batsman),recent_batsman)),row.names=FALSE);
bind_bowl = as.data.frame(t(sapply(as.character(TopBowlers$Bowler),recent_bowler)),row.names=FALSE);
TopBatsmen = cbind(TopBatsmen,bind_bat[,c(3,4)]);
TopBowlers = cbind(TopBowlers,bind_bowl[,c(3,4,5)]);

# Generating Fitness #
TopBatsmen$Fitness = sapply(as.vector(TopBatsmen$Batsman),fitness_bat);
TopBowlers$Fitness = sapply(as.vector(TopBowlers$Bowler),fitness_bowl);
TopBowlers$OpM = round(as.numeric(TopBowlers$Overs)/as.numeric(TopBowlers$Matches),2);

# Generating All Rounders #
AllRounders = as.character(intersect(TopBatsmen$Batsman,TopBowlers$Bowler));
bat_bind = c();
bowl_bind = c();
for(i in 1:length(AllRounders)){
  bat_bind = rbind(bat_bind,TopBatsmen[(TopBatsmen$Batsman==AllRounders[i]),]);
  bowl_bind = rbind(bowl_bind,TopBowlers[(TopBowlers$Bowler==AllRounders[i]),]);
}
TopAllRounders = cbind(bat_bind,bowl_bind[,c(4:33)]);
TopAllRounders$Rating = as.numeric(TopAllRounders$Rating)*as.numeric(TopAllRounders[,52]);
TopAllRounders = TopAllRounders[order(TopAllRounders$Rating,decreasing=TRUE),];

# Analysis #
analysis();

# Generating Venues Information#
Venue_records = as.data.frame(t(sapply(Venues,get_venue)),row.names=FALSE);
Venue_records = Venue_records[order(as.numeric(Venue_records$Matches),decreasing=TRUE),];
WorldCup_Venue_records = as.data.frame(t(sapply(WorldCup_Venues,get_venue)),row.names=FALSE);
WorldCup_Venue_records = WorldCup_Venue_records[order(as.numeric(WorldCup_Venue_records$Matches),decreasing=TRUE),];

