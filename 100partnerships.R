partnerships = function(batsman){
  partnerships = Partnership_records$Total.Runs[((Partnership_records$Batsman1==batsman)|(Partnership_records$Batsman2==batsman))];
  num = as.numeric(sum(partnerships>100));
  return(num);
}