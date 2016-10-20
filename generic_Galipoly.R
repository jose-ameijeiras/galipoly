
#Function for thorwing the dices
folderdices=paste(folder,"dices_Galipoly.R",sep="")
source(folderdices)
#Function for things related with the cards
foldercards=paste(folder,"cards_Galipoly.R",sep="")
source(foldercards)

#Script for building houses and hotels
folderbuy=paste(folder,"buy_Galipoly.R",sep="")
#Script for selling properties
foldersell=paste(folder,"sell_Galipoly.R",sep="")
#Script for playing
folderplay=paste(folder,"play_Galipoly.R",sep="")

#Board

windows.options(width=width.window, height=height.window)
par(mar=c(0,0,0,0))
plot(1,1,col=0,xlim=c(0,11),ylim=c(0,11),axes=F,xlab="",ylab="")

segments(0,11,11,11,lwd=2)
segments(11,11,11,0,lwd=2)
segments(0,11,0,0,lwd=2)
segments(0,0,11,0,lwd=2)

segments(1,10,10,10,lwd=2)
segments(10,10,10,1,lwd=2)
segments(1,10,1,1,lwd=2)
segments(1,1,10,1,lwd=2)

for(i in 1:10){
segments(0,i,1,i)
segments(10,i,11,i)
segments(i,0,i,1)
segments(i,10,i,11)
}

#########################################################################

#Colour in the top

for (i in 0:10) {
x=c(10-i,10-i,11-i,11-i)
y=c(0.8,1,1,0.8)
#polygon(x,y,col="white")
if(i%in%c(6,8,9)){polygon(x,y,col="cadetblue2")}
if(i%in%c(1,3)){polygon(x,y,col="brown")}
}


for (i in 11:20) {
x=c(0,0,1,1)
y=c(i-9.2,i-9,i-9,i-9.2)
#polygon(x,y,col="white")
if(i%in%c(11,13,14)){polygon(x,y,col="pink")}
if(i%in%c(16,18,19)){polygon(x,y,col="orange")}
}

for (i in 21:30) {
x=c(i-20,i-20,i-19,i-19)
y=c(10.8,11,11,10.8)
#polygon(x,y,col="white")
if(i%in%c(21,23,24)){polygon(x,y,col="red")}
if(i%in%c(26,27,29)){polygon(x,y,col="yellow")}
}

for (i in 31:39) {
x=c(10,10,11,11)
y=c(40.8-i,41-i,41-i,40.8-i)
#polygon(x,y,col="white")
if(i%in%c(31,32,34)){polygon(x,y,col="green")}
if(i%in%c(37,39)){polygon(x,y,col="blue")}
}


########################################################

#Prices of the streets (starting from go)


allstn=c(namego,nstreetbr[1],namecomchest,nstreetbr[2],ntaxes[1],nstations[1],
nstreetlb[1],namechance,nstreetlb[2:3],njails[1],
nstreetpi[1],ncompany[1],nstreetpi[2:3],nstations[2],nstreetor[1],
namecomchest,nstreetor[2:3],nfreeparking,nstreetre[1],namechance,
nstreetre[2:3],nstations[3],nstreetye[1:2],ncompany[2],nstreetye[3],
njails[2],nstreetgr[1:2],namecomchest,nstreetgr[3],nstations[4],namechance,
nstreetdb[1],ntaxes[2],nstreetdb[2])

#Separating the words
allstn2=strsplit(allstn," ")

#Prices

pricesst=-c(-20,6,NA,6,20,20,10,NA,10,12,
NA,14,15,14,16,20,18,NA,18,20,
NA,22,NA,22,24,20,26,26,15,28,
NA,30,30,NA,32,20,NA,35,10,40)
pricesst=pricesst*factorcoin



#Prices house and hotel
priceshouse=c(rep(5,5),rep(10,6),rep(15,6),rep(20,5))
priceshouse=priceshouse*factorcoin

#Mortgaging
pricesmort=c(3,3,5,5,6,7,7,8,9,9,10,11,11,12,13,13,14,15,15,16,17.5,20)
pricesmort=pricesmort*factorcoin

#Rent (number of houses; 5=hotel)
pricesrent0=c(0.2,0.4,0.6,0.6,0.8,1,1,1.2,1.4,1.4,1.6,1.8,1.8,2,2.2,2.2,2.4,
2.6,2.6,2.8,3.5,5)
pricesrent1=pricesrent0*5+c(rep(0,19),1,0,-5)
pricesrent2=pricesrent1*3-c(rep(0,4),2,rep(0,3),rep(1,2),rep(2,3),rep(0,7),2.5,0)
pricesrent3=pricesrent2*3-c(rep(0,7),4,5,5,6,5,5,15,19,19,23,27,27,35,40,40)
pricesrent4=c(16,32,40,40,45,62.5,62.5,70,75,75,80,
87.5,87.5,92.5,97.5,97.5,102.5,110,110,120,130,170)
pricesrent5=c(25,45,55,55,60,75,75,90,95,95,100,105,105,110,115,115,120,
127.5,127.5,140,150,200)

pricesrent0=pricesrent0*factorcoin
pricesrent1=pricesrent1*factorcoin
pricesrent2=pricesrent2*factorcoin
pricesrent3=pricesrent3*factorcoin
pricesrent4=pricesrent4*factorcoin
pricesrent5=pricesrent5*factorcoin

#Monopolies
monop=list()
monop[[1]]=c(2,4)
monop[[2]]=c(7,9,10)
monop[[3]]=c(12,14,15)
monop[[4]]=c(17,19,20)
monop[[5]]=c(22,24,25)
monop[[6]]=c(27,28,30)
monop[[7]]=c(32,33,35)
monop[[8]]=c(38,40)

locationsrent=c(0,1,0,2,0,0,3,0,4,5,0,6,0,7,8,0,9,0,10,11,
0,12,0,13,14,0,15,16,0,17,0,18,19,0,20,0,0,21,0,22)

###########################################################################

#Plotting the names of the streets in the board

wordtemp1=numeric()
wordtemp2=numeric()
wordtemp3=numeric()

for(i in 1:40){

#if it has more than one word, it is splitted in two lines

if(length(allstn2[[i]])>1){
cutpoint=nchar(allstn[i])/2
sumofwords=cumsum(nchar(allstn2[[i]]))+1:length(allstn2[[i]])-1
whichgreater=which(sumofwords>=cutpoint)[1]
if(whichgreater==length(allstn2[[i]])){whichgreater=whichgreater-1}
wordtemp1[i]=paste(allstn2[[i]][1:whichgreater],collapse=" ")
wordtemp2[i]=paste(allstn2[[i]][(whichgreater+1):length(allstn2[[i]])],collapse=" ")
}else{
wordtemp1[i]=allstn2[[i]]
wordtemp2[i]=""
if(i==1){
wordtemp2[i]=sentencein1
}
}

#Print the price of the street

if(!is.na(pricesst[i])){
wordtemp3[i]=paste(abs(pricesst[i]),coin,collapse=" ")
}else{
wordtemp3[i]=""
}

}

#Adapt size of the text
maxchar=max(nchar(c(wordtemp1,wordtemp2,wordtemp3)))
spaceocup=maxchar*3
l=dev.size("cm")[1]/spaceocup

for(i in 1:40){

if(i<=10){
text(11.5-i,0.7,wordtemp1[i],cex=l)
text(11.5-i,0.45,wordtemp2[i],cex=l)
text(11.5-i,0.2,wordtemp3[i],cex=l)
}
if(i>10&i<=20){
text(0.5,0.7+i-11,wordtemp1[i],cex=l)
text(0.5,0.45+i-11,wordtemp2[i],cex=l)
text(0.5,0.2+i-11,wordtemp3[i],cex=l)
}
if(i>20&i<=30){
text(0.5+i-21,10.7,wordtemp1[i],cex=l)
text(0.5+i-21,10.45,wordtemp2[i],cex=l)
text(0.5+i-21,10.2,wordtemp3[i],cex=l)
}
if(i>30){
text(10.5,0.7-i+41,wordtemp1[i],cex=l)
text(10.5,0.45-i+41,wordtemp2[i],cex=l)
text(10.5,0.2-i+41,wordtemp3[i],cex=l)
}

}


#########################################################################

#Position; Money; Chance and Community Chest cards; Jail; Doubles

#Position of the players
positions=rep(1,nplayers)

#Jail counter
jail=rep(0,nplayers)

#Doubles counter
doubles=rep(0,nplayers)

#Initial money
money=rep(150*factorcoin,nplayers)

#Number of houses (5=hotel)
houses=rep(0,40)

#Owner of the property
property=rep(0,40)

#Possible movements (chance)
ch=c(0,-3,16,25,0,0,1,0,0,0,0,12,40,31,0,-2000)
#Collected and payed money (chance)
chm=c(15,0,0,0,-1.5,5,0,-1000,-1001,0,0,0,0,0,0,0)*factorcoin
#Without the jail card (chance)
ch2=c(0,-3,16,25,0,0,1,0,0,0,0,12,40,31,0)
chm2=c(15,0,0,0,-1.5,5,0,-1000,-1001,0,0,0,0,0,0)*factorcoin

#Possible movements (community chest)
cc=c(0,0,31,0,1,0,0,0,0,0,0,0,0,0,2,-2000)
#Collected and payed money (community chest)
ccm=c(-10,-15,0,-1002,0,20,1,10,-10,-1001,0,0,0,0,0,0)*factorcoin
#Without the jail card (community chest)
cc2=c(0,0,31,0,1,0,0,0,0,0,0,0,0,0,2)
ccm2=c(-10,-15,0,-1002,0,20,1,10,-10,-1001,0,0,0,0,0)*factorcoin

# -2000 get out of jail free
# -1000 pay each player 2.5 
# -1001 pay 2.5 per house and 10 per hotel
# -1002 collect 5 from every player

#Who has the jail card?
chjail=rep(0,nplayers)
ccjail=rep(0,nplayers)

#if it has more than one word, it is splitted in two lines

chancewords2=strsplit(chancewords," ")
comchwords2=strsplit(comchwords," ")

ccwordtemp1=numeric()
ccwordtemp2=numeric()
chwordtemp1=numeric()
chwordtemp2=numeric()

for(i in 1:length(chancewords)){
if(length(chancewords2[[i]])>1){
cutpoint=nchar(chancewords[i])/2
sumofwords=cumsum(nchar(chancewords2[[i]]))+1:length(chancewords2[[i]])-1
whichgreater=which(sumofwords>=cutpoint)[1]
if(whichgreater==length(chancewords2[[i]])){whichgreater=whichgreater-1}
chwordtemp1[i]=paste(chancewords2[[i]][1:whichgreater],collapse=" ")
chwordtemp2[i]=paste(chancewords2[[i]][(whichgreater+1):length(chancewords2[[i]])],collapse=" ")
}else{
chwordtemp1[i]=chancewords2[[i]]
chwordtemp2[i]=""
}
}

for(i in 1:length(comchwords)){
if(length(comchwords2[[i]])>1){
cutpoint=nchar(comchwords[i])/2
sumofwords=cumsum(nchar(comchwords2[[i]]))+1:length(comchwords2[[i]])-1
whichgreater=which(sumofwords>=cutpoint)[1]
if(whichgreater==length(comchwords2[[i]])){whichgreater=whichgreater-1}
ccwordtemp1[i]=paste(comchwords2[[i]][1:whichgreater],collapse=" ")
ccwordtemp2[i]=paste(comchwords2[[i]][(whichgreater+1):length(comchwords2[[i]])],collapse=" ")
}else{
ccwordtemp1[i]=comchwords2[[i]]
ccwordtemp2[i]=""
}
}

#Adapt size of the text
maxchar1=max(nchar(c(ccwordtemp1,ccwordtemp2,chwordtemp1,chwordtemp2)))
maxchar2=max(nchar(cchmonwords[1:2]))+nchar(factorcoin)+nchar(coin)+nchar(cchmonwords[4])+3
maxchar3=nchar(factorcoin)+nchar(coin)+nchar(cchmonwords[5])+4
maxchar4=nchar(cchmonwords[3])
maxchar5=nchar(cchmonwords[1])+nchar(factorcoin)+nchar(coin)+2
maxchar=max(maxchar1,maxchar2,maxchar3,maxchar4,maxchar5)
spaceocup=maxchar*1.3
l3=dev.size("cm")[1]/spaceocup




##################################################################

#Chance and Community Chest cards (before start)

#Shuffle
chpos=sample(1:16,size=16,replace=FALSE)
ccpos=sample(1:16,size=16,replace=FALSE)
ch=ch[chpos]
cc=cc[ccpos]
chm=chm[chpos]
ccm=ccm[ccpos]
counterch=0
countercc=0

#Plot in the board

#Adjust size of the text
maxchar=max(nchar(c(wordtemp1[c(3,8)],wordtemp2[c(3,8)])))
spaceocup=maxchar*2.2
l2=dev.size("cm")[1]/spaceocup

plotbackcards=function(){
x=c(2,2,4,4)
y=c(7,9,9,7)
polygon(x,y,col="white")
text(3,8.5,toupper(wordtemp1[8]),cex=l2)
text(3,7.5,toupper(wordtemp2[8]),cex=l2)


x=c(7,7,9,9)
y=c(2,4,4,2)
polygon(x,y,col="white")
text(8,3.5,toupper(wordtemp1[3]),cex=l2)
text(8,2.5,toupper(wordtemp2[3]),cex=l2)
}
plotbackcards()

#Should the jail card be removed? (No=1,Yes=2)
chused=1
ccused=1



#####################################################################

#Money of the players printed on the board


#Colour of the player
colplayer=1:nplayers

#Name players
namep2=numeric()
for(i in 1:nplayers){
namep2[i]=paste(namegen,i)
}
if(!sum(namep==namep2)==nplayers){
namep=sample(namep)
}

maxchar=max(nchar(namep))+nchar(namemoney)+nchar(coin)+7+nchar(factorcoin)
spaceocup=maxchar*0.9
l4=dev.size("cm")[1]/spaceocup


moneyfunc=function(){
x=c(2,2,6.5,6.5)
y=c(2,6.5,6.5,2)
polygon(x,y,col="white")
for(i in 1:nplayers){
maxirep=nchar(money[i])%%3
if(maxirep==0){maxirep=3}
if(abs(money[i])>999){
text(4.3,6.5-0.5*i,paste(i,":",namep[i],".",namemoney,paste(substring(money[i],c(1,seq(maxirep+1,nchar(money[i])-2,3)),seq(maxirep,nchar(money[i]),3)),collapse=" "),coin),cex=l4)
}else{
text(4.3,6.5-0.5*i,paste(i,":",namep[i],".",namemoney,money[i],coin),cex=l4)
}
x=c(2.2,2.2,2.7,2.7)
y=c(6.5-0.5*i+0.2,6.5-0.5*i-0.2,6.5-0.5*i-0.2,6.5-0.5*i+0.2)
polygon(x,y,col=colplayer[i])
}
}
moneyfunc()


########################################################################

#Position of the players (plotted on the board)

#Token
pi=3.141592
tita=seq(0,2*pi,by=0.01)
x3=cos(tita)
y3=sin(tita)
#To see all the tokens
tokenx=c(rep(0.21,2),rep(-0.21,2),rep(0.1,2),rep(-0.1,2))
tokeny=rep(c(0.05,-0.2),4)

tokenfunc=function(player){
if(positions[player]<=10){
polygon(tokenx[player]+11.5-positions[player]+0.25*x3,tokeny[player]+0.5+0.25*y3,col=colplayer[player])
}
if((positions[player]>=11)&(positions[player]<=20)){
polygon(tokenx[player]+0.5+0.25*x3,tokeny[player]-10.5+positions[player]+0.25*y3,col=colplayer[player])
}
if((positions[player]>=21)&(positions[player]<=30)){
polygon(tokenx[player]-0.5+positions[player]-20+0.25*x3,tokeny[player]+10.5+0.25*y3,col=colplayer[player])
}
if(positions[player]>=31){
polygon(tokenx[player]+10.5+0.25*x3,tokeny[player]+11.5-positions[player]+30+0.25*y3,col=colplayer[player])
}
}

for(j in 1:nplayers){
tokenfunc(j)
}


########################################################################

#Houses and hotels (plot)

#Shape of the houses and hotels
housesshape=function(positx,posity){

x=c(positx-0.1,positx-0.1,positx+0.1,positx+0.1)
y=c(posity-0.1,posity+0.1,posity+0.1,posity-0.1)
polygon(x,y,col="green",border=NA)

x=c(positx-0.2,positx,positx+0.2)
y=c(posity+0.1,posity+0.25,posity+0.1)
polygon(x,y,col="green",border=NA)

}

hotelsshape=function(positx,posity){

x=c(positx-0.2,positx-0.2,positx+0.2,positx+0.2)
y=c(posity-0.2,posity+0.2,posity+0.2,posity-0.2)
polygon(x,y,col="red",border=NA)

x=c(positx-0.35,positx,positx+0.35)
y=c(posity+0.2,posity+0.5,posity+0.2)
polygon(x,y,col="red",border=NA)

}


allhouses=function(positx,posity,number){

if(number==1){
housesshape(positx,posity)
}

if(number==2){
housesshape(positx-0.25,posity)
housesshape(positx+0.25,posity)
}

if(number==3){
housesshape(positx-0.25,posity+0.25)
housesshape(positx+0.25,posity+0.25)
housesshape(positx,posity-0.1)
}
if(number==4){
housesshape(positx-0.25,posity+0.25)
housesshape(positx+0.25,posity+0.25)
housesshape(positx-0.25,posity-0.1)
housesshape(positx+0.25,posity-0.1)
}

if(number==5){
hotelsshape(positx,posity)
}

}




housesfunc=function(posit,number){

if(posit<=10){
allhouses(11.5-posit,0.25,number)
}
if(posit>10&posit<=20){
allhouses(0.5,-10.75+posit,number)
}
if(posit>20&posit<=30){
allhouses(-20.5+posit,10.25,number)
}

if(posit>30){
allhouses(10.5,41.25-posit,number)
}

}


#########################################################################

#When something happens in a box replot it

boxfunc=function(posit){


if(posit<=10){
#Box
x=c(11-posit,11-posit,12-posit,12-posit)
y=c(0,1,1,0)
polygon(x,y,col="white")
#Text in the box
text(11.5-posit,0.7,wordtemp1[posit],cex=l)
text(11.5-posit,0.45,wordtemp2[posit],cex=l)
text(11.5-posit,0.2,wordtemp3[posit],cex=l)
#Colour of the street
y=c(0.8,1,1,0.8)
if(posit%in%c(7,9,10)){polygon(x,y,col="cadetblue2")}
if(posit%in%c(2,4)){polygon(x,y,col="brown")}
#Owner of the street
if(property[posit]>0){
x=c(11.5-posit,11.5-posit,12-posit,12-posit)
polygon(x,y,col=colplayer[(property[posit])])
text(11.75-posit,0.9,property[posit],col="white",cex=l)
#Houses in the street
if(houses[posit]>0){
housesfunc(posit,houses[posit])
}
}
#Plot the tokens of the players
if(sum(positions==posit)>0){
whicharehere=which(positions==posit)
for(j in whicharehere){
tokenfunc(j)
}
}

}


if(posit>10&posit<=20){
x=c(0,0,1,1)
y=c(posit-11,posit-10,posit-10,posit-11)
polygon(x,y,col="white")
text(0.5,0.7+posit-11,wordtemp1[posit],cex=l)
text(0.5,0.45+posit-11,wordtemp2[posit],cex=l)
text(0.5,0.2+posit-11,wordtemp3[posit],cex=l)
y=c(posit-10.2,posit-10,posit-10,posit-10.2)
#polygon(x,y,col="white")
if(posit%in%c(12,14,15)){polygon(x,y,col="pink")}
if(posit%in%c(17,19,20)){polygon(x,y,col="orange")}
if(property[posit]>0){
x=c(0.5,0.5,1,1)
polygon(x,y,col=colplayer[(property[posit])])
text(0.75,posit-10.1,property[posit],col="white",cex=l)
if(houses[posit]>0){
housesfunc(posit,houses[posit])
}
}
if(sum(positions==posit)>0){
whicharehere=which(positions==posit)
for(j in whicharehere){
tokenfunc(j)
}
}
}


if(posit>20&posit<=30){
x=c(posit-21,posit-21,posit-20,posit-20)
y=c(10,11,11,10)
polygon(x,y,col="white")
text(0.5+posit-21,10.7,wordtemp1[posit],cex=l)
text(0.5+posit-21,10.45,wordtemp2[posit],cex=l)
text(0.5+posit-21,10.2,wordtemp3[posit],cex=l)
y=c(10.8,11,11,10.8)
if(posit%in%c(22,24,25)){polygon(x,y,col="red")}
if(posit%in%c(27,28,30)){polygon(x,y,col="yellow")}
if(property[posit]>0){
x=c(posit-20.5,posit-20.5,posit-20,posit-20)
polygon(x,y,col=colplayer[(property[posit])])
text(-20.25+posit,10.9,property[posit],col="white",cex=l)
if(houses[posit]>0){
housesfunc(posit,houses[posit])
}
}
if(sum(positions==posit)>0){
whicharehere=which(positions==posit)
for(j in whicharehere){
tokenfunc(j)
}
}
}


if(posit>30){
x=c(10,10,11,11)
y=c(41-posit,42-posit,42-posit,41-posit)
polygon(x,y,col="white")
text(10.5,0.7-posit+41,wordtemp1[posit],cex=l)
text(10.5,0.45-posit+41,wordtemp2[posit],cex=l)
text(10.5,0.2-posit+41,wordtemp3[posit],cex=l)
y=c(41.8-posit,42-posit,42-posit,41.8-posit)
if(posit%in%c(32,33,35)){polygon(x,y,col="green")}
if(posit%in%c(38,40)){polygon(x,y,col="blue")}
if(property[posit]>0){
x=c(10.5,10.5,11,11)
polygon(x,y,col=colplayer[(property[posit])])
text(10.75,41.9-posit,property[posit],col="white",cex=l)
if(houses[posit]>0){
housesfunc(posit,houses[posit])
}
}
if(sum(positions==posit)>0){
whicharehere=which(positions==posit)
for(j in whicharehere){
tokenfunc(j)
}
}
}

}


###################################################################

#How much do you have to pay

payfunc=function(player){

posit=positions[player]

#In positions 5 and 39 you always pay
if(posit==5){
money[player]=money[player]-20*factorcoin
}
if(posit==39){
money[player]=money[player]-100*factorcoin
}

#If the property belongs to another player and it is not an station nor an company

if((posit%%10)!=6 & (!posit%in%c(13,29))){
if((property[posit]>0)&(property[posit]!=player)){
if(houses[posit]==0){
money[player]=money[player]-pricesrent0[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent0[(locationsrent[posit])]
#If the other player has the monopoly and no edifications, pay twice
ismonop1=lapply(monop,posit,FUN="%in%")
ismonop2=which(sapply(ismonop1,FUN=sum)==1)
ismonop3=monop[[ismonop2]]
ismonop4=sum(property[ismonop3]==property[posit])==(c(2,rep(3,6),2)[ismonop2])
ismonop5=sum(houses[ismonop3])==0
if(ismonop4&ismonop5){
money[player]=money[player]-pricesrent0[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent0[(locationsrent[posit])]
}
}
if(houses[posit]==1){
money[player]=money[player]-pricesrent1[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent1[(locationsrent[posit])]
}
if(houses[posit]==2){
money[player]=money[player]-pricesrent2[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent2[(locationsrent[posit])]
}
if(houses[posit]==3){
money[player]=money[player]-pricesrent3[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent3[(locationsrent[posit])]
}
if(houses[posit]==4){
money[player]=money[player]-pricesrent4[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent4[(locationsrent[posit])]
}
if(houses[posit]==5){
money[player]=money[player]-pricesrent5[(locationsrent[posit])]
money[property[posit]]=money[property[posit]]+pricesrent5[(locationsrent[posit])]
}
}
}

#If it is an station
if(((posit%%10)==6)&(property[posit]>0)&(property[posit]!=player)){
coststations=c(2.5,5,10,20)*factorcoin
howmanystat=sum(property[c(6,16,26,36)]==property[posit])
money[player]=money[player]-coststations[howmanystat]
money[property[posit]]=money[property[posit]]+coststations[howmanystat]
}

#If it is a company
if((posit%in%c(13,29))&(property[posit]>0)&(property[posit]!=player)){
positions2=positions
paycompan1=dicefunc(player)
paycompan2=paycompan1[[2]]+paycompan1[[3]]
if(sum(property[c(13,29)]==property[posit])==1){
paycompan3=4*paycompan2*factorcoin/10}
if(sum(property[c(13,29)]==property[posit])==2){
paycompan3=paycompan2*factorcoin}
positions=positions2
money[player]=money[player]-paycompan3
money[property[posit]]=money[property[posit]]+paycompan3
}

return(money)

}






