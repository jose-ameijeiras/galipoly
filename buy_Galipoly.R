#Build houses and hotels
#
#Has this player monopolies?
propplayer=which(property==player)
ismonop=lapply(monop,propplayer,FUN="%in%")
totalmonop1=sapply(ismonop,FUN=sum)==c(2,rep(3,6),2)
totalmonop2=sum(totalmonop1)
if(totalmonop2>0){
totalmonop3=numeric()
#print(paste(namep[player],msgbuild1,sep=" "))
for(ij in which(totalmonop1==T)){
for(ik in monop[[ij]]){
#print(paste(ik,allstn[ik],sep=": "))
totalmonop3=c(totalmonop3,ik)
}
}
#cat(msgbuild2)
isbuilt1=GI5()
if(isbuilt1>0){isbuilt1=totalmonop3[isbuilt1]}

if((isbuilt1>0)&(isbuilt1%in%totalmonop3)){
if(houses[isbuilt1]<5){
#print(paste(msgbuild3,allstn[isbuilt1],msgbuild4,houses[isbuilt1],msgbuild5,sep=" "))
#cat(msgbuild7)
isbuilt2=GI6()+houses[isbuilt1]
if((isbuilt2>houses[isbuilt1])&(isbuilt2<=5)){
money[player]=money[player]-priceshouse[locationsrent[isbuilt1]]*isbuilt2
money[player]=money[player]+priceshouse[locationsrent[isbuilt1]]*houses[isbuilt1]
houses[isbuilt1]=isbuilt2
moneyfunc()
boxfunc(isbuilt1)
}else{
#print(msgbuild9)
}
}else{
#print(paste(msgbuild3,allstn[isbuilt1],msgbuild4,1,msgbuild6,sep=" "))
}
}
if((isbuilt1>0)&(!isbuilt1%in%totalmonop3)){
#print(msgbuild8)
}

if(isbuilt1>0){
#Repeat the buy process
repeatbuy=1
}else{
repeatbuy=0
}
}else{
repeatbuy=0
}