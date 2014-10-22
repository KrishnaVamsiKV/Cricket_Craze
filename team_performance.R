team_performance = function(Country){
  Team = data.frame(matrix(nrow=1,ncol=7));
  names(Team) = c("Country","Matches","Won","Batting1","Batting1.Win",
                  "Batting2","Batting2.Win");
  Team$Country = Country;
  Team$Matches = as.numeric(Team_records$Matches[(Team_records$Country==Country)]);
  Team$Won = as.numeric(Team_records$Won[(Team_records$Country==Country)]);
  Team$Batting1 = as.numeric(Team_records$Batting1[(Team_records$Country==Country)]);
  Team$Batting2 = as.numeric(Team_records$Batting2[(Team_records$Country==Country)]);
  teamdata = Match_records[((Match_records$Team1==Country)|(Match_records$Team2==Country)),];
  Team$Batting1.Win =  nrow(teamdata[((teamdata$Batting.Team==Country)
                                      &(teamdata$Innings==1)&(teamdata$Win==Country)),]);
  Team$Batting2.Win =  nrow(teamdata[((teamdata$Batting.Team==Country)
                                      &(teamdata$Innings==2)&(teamdata$Win==Country)),]);
  return(Team);
}