team_info = function(Country){
  Team = data.frame(matrix(nrow=1,ncol=20));
  names(Team) = c("Country","Matches","Won","Batting1","Batting2","Toss.Won","Batting.Chosen","Fielding.Chosen","Avg.Runs.Batting1","Run.Rate.Batting1","Avg.Runs.Batting2","Run.Rate.Batting2","Avg.Wickets.Batting1","Avg.Wickets.Batting2","Avg.Runs.Bowling1","Run.Rate.Bowling1","Avg.Runs.Bowling2","Run.Rate.Bowling2","Avg.Wickets.Bowling1","Avg.Wickets.Bowling2");
  teamdata = Match_records[((Match_records$Team1==Country)|(Match_records$Team2==Country)),];
  Team$Country = Country;
  Team$Matches = length(unique(teamdata$Match.No));
  Team$Won = nrow(teamdata[(teamdata$Win==Country),])/2;
  Team$Batting1 = nrow(teamdata[((teamdata$Batting.Team==Country)
                                 &(teamdata$Innings==1)),]);
  Team$Batting2 = nrow(teamdata[((teamdata$Batting.Team==Country)
                                 &(teamdata$Innings==2)),]);
  Team$Toss.Won = ceiling(nrow(teamdata[(teamdata$Toss.Winner==Country),])/2);
  Team$Batting.Chosen = ceiling(nrow(teamdata[((teamdata$Toss.Winner==Country)
                                       &(teamdata$Toss.Decision=="bat")),])/2);
  Team$Fielding.Chosen = ceiling(nrow(teamdata[((teamdata$Toss.Winner==Country)
                                        &(teamdata$Toss.Decision=="field")),])/2);
  Team$Run.Rate.Batting1 = round(mean(teamdata$Run.Rate[((teamdata$Batting.Team==Country)
                                                     &(teamdata$Innings==1))]),2);
  Team$Run.Rate.Batting2 = round(mean(teamdata$Run.Rate[((teamdata$Batting.Team==Country)
                                                     &(teamdata$Innings==2))]),2);
  Team$Avg.Runs.Batting1 = round(mean(teamdata$Total.Runs[((teamdata$Batting.Team==Country)
                                                     &(teamdata$Innings==1))]),2);
  Team$Avg.Runs.Batting2 = round(mean(teamdata$Total.Runs[((teamdata$Batting.Team==Country)
                                                     &(teamdata$Innings==2))]),2);
  Team$Avg.Runs.Bowling1 = round(mean(teamdata$Total.Runs[((teamdata$Batting.Team!=Country)
                                                     &(teamdata$Innings==1))]),2);
  Team$Avg.Runs.Bowling2 = round(mean(teamdata$Total.Runs[((teamdata$Batting.Team!=Country)
                                                     &(teamdata$Innings==2))]),2);
  Team$Run.Rate.Bowling1 = round(mean(teamdata$Run.Rate[((teamdata$Batting.Team!=Country)
                                                   &(teamdata$Innings==1))]),2);
  Team$Run.Rate.Bowling2 = round(mean(teamdata$Run.Rate[((teamdata$Batting.Team!=Country)
                                                   &(teamdata$Innings==2))]),2);
  Team$Avg.Wickets.Batting1 = round(mean(teamdata$Wicket.Kind.Total[((teamdata$Batting.Team==Country)
                                                               &(teamdata$Innings==1))]),2);
  Team$Avg.Wickets.Batting2 = round(mean(teamdata$Wicket.Kind.Total[((teamdata$Batting.Team==Country)
                                                               &(teamdata$Innings==2))]),2);
  Team$Avg.Wickets.Bowling1 = round(mean(teamdata$Wicket.Kind.Total[((teamdata$Batting.Team!=Country)
                                                               &(teamdata$Innings==1))]),2);
  Team$Avg.Wickets.Bowling2 = round(mean(teamdata$Wicket.Kind.Total[((teamdata$Batting.Team!=Country)
                                                               &(teamdata$Innings==2))]),2);
  
  return(Team);
}