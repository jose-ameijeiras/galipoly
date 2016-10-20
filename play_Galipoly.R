GI4()


#If you remains in jail
if(jail[player]==1){

jailfree=GI10()
if(jailfree==1){
money[player]=money[player]-5*factorcoin
moneyfunc()
jail[player]=0
}
if(jailfree==2){
chjail[player]=0
jail[player]=0
}
if(jailfree==3){
ccjail[player]=0
jail[player]=0
}

}



#Throw dices and pick the card 

plotbackcards()
prevposition1=positions[player]
throwdices=dicefunc(player)
if((jail[player]==0)|(jail[player]==1 & (throwdices[[2]]==throwdices[[3]]))){

positions=throwdices[[1]]

#Was this player in the jail
if(jail[player]==1){
prevjail=1
jail[player]=0
}else{
prevjail=0
}

boxfunc(prevposition1)
boxfunc(positions[player])
if(positions[player]%in%c(8,23,37,3,18,34)){
prevposition2=positions[player]
whathappenscards=cardsfunc(player)
ch=whathappenscards[[1]]
cc=whathappenscards[[2]]
chm=whathappenscards[[3]]
ccm=whathappenscards[[4]]
ch2=whathappenscards[[5]]
cc2=whathappenscards[[6]]
chm2=whathappenscards[[7]]
ccm2=whathappenscards[[8]]
counterch=whathappenscards[[9]]
countercc=whathappenscards[[10]]
positions=whathappenscards[[11]]
money=whathappenscards[[12]]
chjail=whathappenscards[[13]]
ccjail=whathappenscards[[14]]
chused=whathappenscards[[15]]
ccused=whathappenscards[[16]]
boxfunc(prevposition2)
boxfunc(positions[player])
}

if((prevposition1>positions[player])&(positions[player]!=31)){
money[player]=money[player]+20*factorcoin
}

money=payfunc(player)

moneyfunc()



#Buy properties (including stations)

if(((locationsrent[(positions[player])]>0)|((positions[player]%%10)==6)|(positions[player]%in%c(13,29)))&(property[(positions[player])]==0)){

#Sell (and motgage) properties
repeatsell=1
while(repeatsell==1){
source(foldersell)
}

#print(paste(msgtobuy1,allstn[(positions[player])],msgtobuy2,sep=""))
decission=GI11()
if(decission==1){
property[(positions[player])]=player
money[player]=money[player]+pricesst[(positions[player])]
moneyfunc()
boxfunc(positions[player])
}
}

#If doubles->another turn, three doubles->jail 
if((throwdices[[2]]==throwdices[[3]])&(prevjail==0)){
doubles[player]=doubles[player]+1
GI12()
if(doubles[player]==3){
positions[player]=31
doubles[player]=0
GI13()
}else{
playerrep=1
}

}else{
playerrep=0
doubles[player]=0
}

#If your position is 31, then go to jail
if(positions[player]==31){
jail[player]=1
prevjailplayer=positions[player]
positions[player]=11
boxfunc(prevjailplayer)
boxfunc(positions[player])
}


}

if(money[player]<=0 & warningout[player]==0){
GI18()
warningout[player]=1
}else{
warningout[player]=0
}

#Buy houses and hotels
repeatbuy=1
while(repeatbuy==1){
source(folderbuy)
}

#Sell (and motgage) properties
repeatsell=1
while(repeatsell==1){
source(foldersell)
}

if(money[player]>0 & warningout[player]==1){
warningout[player]=0
}

endit=GI14()
if(endit==1){
endofgame=1
GI15()
}
if(endit==2|(money[player]<=0)){
playerout[player]=1
money[player]=0
property[property==player]=0
houses[property==player]=0
moneyfunc()
GI16()
}
if(sum(playerout==1)>=(nplayers-1)){
endofgame=1
GI19()
}


#Which player is going to play
player=player+1
if(playerrep==1){
playerrep=0
player=player-1
}
if(player==(nplayers+1)){player=1}


while(playerout[player]==1){
player=player+1
if(player==(nplayers+1)){player=1}
}

