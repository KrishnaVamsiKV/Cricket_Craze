get_mom = function(player){
  num = length(unique(Cricket$Match.No[(Cricket$Man.of.the.Match==player)]));
  return(num);
}