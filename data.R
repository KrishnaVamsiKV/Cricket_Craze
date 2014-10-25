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

# Generating batsmen records #
Batsmen_records =  as.data.frame(t(sapply(Batsmen,batsmen_record)),row.names=FALSE);

# Generating bowlers records #
Bowlers_records =  as.data.frame(t(sapply(Bowlers,bowler_record)),row.names=FALSE);

# Generating Partnership information #
Partnership_records=get_partnership(Cricket);

# Generating Match Information#
Match_records = get_matches(Cricket);

# Generating Teams Performance #
Team_records = as.data.frame(t(sapply(Teams,team_info)),row.names=FALSE);
Team_performance = as.data.frame(t(sapply(Teams,team_performance)),row.names=FALSE);

# Generating Consistence Criteria #
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
rating = (1+4*(as.numeric(Batsmen_Consistency$Average.Mean)>=42.41)
          +3*((as.numeric(Batsmen_Consistency$Average.Mean)<42.41)&(as.numeric(Batsmen_Consistency$Average.Mean)>=37.15))
          +2*((as.numeric(Batsmen_Consistency$Average.Mean)<37.15)&(as.numeric(Batsmen_Consistency$Average.Mean)>=35.67))
          +1*((as.numeric(Batsmen_Consistency$Average.Mean)<35.67)&(as.numeric(Batsmen_Consistency$Average.Mean)>=30.80)))
Batsmen_Consistency$Rating = round((as.numeric(Batsmen_Consistency$Average.Mean)^2*(rating*as.numeric(Batsmen_Consistency$Strike.Rate.Mean))*as.numeric(Batsmen_Consistency$streaks))
                                     /(as.numeric(Batsmen_Consistency$Average.Sd)),2);
 
Batsmen_Consistency = Batsmen_Consistency[order(Batsmen_Consistency$Rating,decreasing=TRUE),];


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
rating = (1+4*(as.numeric(Bowlers_Consistency$Average.Mean)<=24.87)
          +3*((as.numeric(Bowlers_Consistency$Average.Mean)<28.40)&(as.numeric(Bowlers_Consistency$Average.Mean)>=24.87))
          +2*((as.numeric(Bowlers_Consistency$Average.Mean)<29.36)&(as.numeric(Bowlers_Consistency$Average.Mean)>=28.40))
          +1*((as.numeric(Bowlers_Consistency$Average.Mean)<33.93)&(as.numeric(Bowlers_Consistency$Average.Mean)>=29.36)))

Bowlers_Consistency$Rating = round(((as.numeric(Bowlers_Consistency$Dot.Percent.Mean))*(rating)*(as.numeric(Bowlers_Consistency$Streaks)))/
                                     ((as.numeric(Bowlers_Consistency$Average.Mean))*(as.numeric(Bowlers_Consistency$Economy.Mean))*(as.numeric(Bowlers_Consistency$Average.Sd))),2);
Bowlers_Consistency$WpM = round((as.numeric(Bowler_Consistency$Economy.Mean))*10/(as.numeric(Bowlers_Consistency$Average.Mean)),2);
Bowlers_Consistency = Bowlers_Consistency[order(Bowlers_Consistency$Rating,decreasing=TRUE),];

