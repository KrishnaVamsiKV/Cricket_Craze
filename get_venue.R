get_venue = function(venue){
  Venue = data.frame(matrix(nrow=1,ncol=20));
  names(Venue) = c("Venue","Matches","Batting1.Win","Batting2.Win","Batting.Chosen","Fielding.Chosen",
                  "Avg.Runs.Batting1","Run.Rate.Batting1","Avg.Runs.Batting2","Run.Rate.Batting2","Avg.Wickets.Batting1","Avg.Wickets.Batting2",
                  "Best.Team1","Best.Points1","Best.Team2","Best.Points2",
                  "Worst.Team1","Worst.Points1","Worst.Team2","Worst.Points2");
  venuedata = Match_records[(Match_records$Venue==venue),];
  Venue$Venue = venue;
  Venue$Matches = length(unique(venuedata$Match.No));
  Venue$Batting1.Win = nrow(venuedata[((venuedata$Innings==1)&(venuedata$Batting.Team==venuedata$Win)),]);
  Venue$Batting2.Win = nrow(venuedata[((venuedata$Innings==2)&(venuedata$Batting.Team==venuedata$Win)),]);
  Venue$Batting.Chosen = length(unique(venuedata$Match.No[(venuedata$Toss.Decision=="bat")]));
  Venue$Fielding.Chosen = length(unique(venuedata$Match.No[(venuedata$Toss.Decision=="field")]));
  Venue$Avg.Runs.Batting1 = round(mean(venuedata$Total.Runs[(venuedata$Innings==1)]),2);
  Venue$Avg.Runs.Batting2 = round(mean(venuedata$Total.Runs[(venuedata$Innings==2)]),2);
  Venue$Run.Rate.Batting1 = round(mean(venuedata$Run.Rate[(venuedata$Innings==1)]),2);
  Venue$Run.Rate.Batting2 = round(mean(venuedata$Run.Rate[(venuedata$Innings==2)]),2);
  Venue$Avg.Wickets.Batting1 = round(mean(venuedata$Wicket.Kind.Total[(venuedata$Innings==1)]),2);
  Venue$Avg.Wickets.Batting2 = round(mean(venuedata$Wicket.Kind.Total[(venuedata$Innings==2)]),2);
  countries = unique(venuedata$Batting.Team);
  wins = data.frame(matrix(nrow=length(countries),ncol=2));
  names(wins) = c("Country","Points");
  wins$Country = countries;
  for(i in 1:length(countries)){
    wins$Points[i] = round(sum(venuedata$points[(venuedata$Win==countries[i])])/length(unique(venuedata$Match.No[((venuedata$Team1==countries[i])|(venuedata$Team2==countries[i]))])),2);
  }
  wins$Points[(is.na(wins$Points))] = 0;
  wins = wins[order(wins$Points,decreasing=TRUE),];
  Venue$Best.Team1 = wins$Country[1];
  Venue$Best.Team2 = wins$Country[2];
  Venue$Best.Points1 = wins$Points[1];
  Venue$Best.Points2 = wins$Points[2];
  Venue$Worst.Team1 = wins$Country[length(countries)];
  Venue$Worst.Team2 = wins$Country[length(countries)-1];
  Venue$Worst.Points1 = wins$Points[length(countries)];
  Venue$Worst.Points2 = wins$Points[length(countries)-1];
 
  return(Venue);
}