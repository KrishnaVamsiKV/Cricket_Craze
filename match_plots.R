test= Cricket[1:604,]
test$score=0
test[1,27]=test[1,23]
for(i in 2:308){
  test[i,27]=test[i-1,27]+test[i,23]
}
test[309,27]=test[309,23]
for(i in 310:604){
  test[i,27]=test[i-1,27]+test[i,23]
}
plot(test[1:308,1],test[1:308,27],type="l")
lines(test[309:604,1],test[309:604,27],type="l",col="green")
for(i in 1:604){
  if(test[i,26]==""){
    test[i,28]=300
  }
  else{
    test[i,28]=test[i,27]
  }
}
points(test[1:308,28],pch=16)
points(test[309:604,28],pch=1)
test$zeroes=0
plot(test[1:308,1],-test[1:308,27]+test[309:604,27],type="l")
lines(test[1:308,1],test[1:308,29],type="l")
test$Req.Run.Rate=0
test[309:604,30]=(test[308,27]+1-test[309:604,27])*6/(300-floor(test[309:604,14])*6-(test[309:604,14]-floor(test[309:604,14]))*10)
plot(test[309:604,1]/6,test[309:604,30],type="l")
test$Pre.Run.Rate=0
test[309:604,31]=  test[309:604,27]*6/(floor(test[309:604,14])*6+(test[309:604,14]-floor(test[309:604,14]))*10)
lines(test[309:604,1]/6,test[309:604,31],type="l",col="blue")
