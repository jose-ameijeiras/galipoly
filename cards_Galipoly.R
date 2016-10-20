

#Chance and Community Chest cards

cardsfunc=function(player){

posit=positions[player]

if(!posit%in%c(8,23,37)){
x=c(2,2,4,4)
y=c(7,9,9,7)
polygon(x,y,col="white")
text(3,8.5,toupper(wordtemp1[8]),cex=l2)
text(3,7.5,toupper(wordtemp2[8]),cex=l2)
}else{

x=c(2,2,4,4)
y=c(7,9,9,7)
polygon(x,y,col="white")

counterch=counterch+1

if(chused==1){chtemp=ch}
if(chused==2){chtemp=ch2}
if(chused==1){chmtemp=chm}
if(chused==2){chmtemp=chm2}

if(chtemp[counterch] == -2000){
chjail[player]=1
text(3,8.75,chwordtemp1[5],cex=l3)
text(3,8.25,chwordtemp2[5],cex=l3)
}
if(chtemp[counterch] == -3){
positions[player]=positions[player]-3
text(3,8.75,chwordtemp1[1],cex=l3)
text(3,8.25,chwordtemp2[1],cex=l3)
}
if(chtemp[counterch] >0){
positions[player]=chtemp[counterch] 
if(chtemp[counterch]==31){
text(3,8.75,chwordtemp1[2],cex=l3)
text(3,8.25,chwordtemp2[2],cex=l3)
}else{
text(3,8.75,chwordtemp1[3],cex=l3)
text(3,8.25,paste(chwordtemp2[3],chtemp[counterch]),cex=l3)
}
}

if((chmtemp[counterch]>(-100*factorcoin))&(chmtemp[counterch]<0)){
text(3,7.75,cchmonwords[2],cex=l3)
text(3,7.25,paste(abs(chmtemp[counterch]),coin),cex=l3)
money[player]=money[player]+chmtemp[counterch]
}
if((chmtemp[counterch]>(-100*factorcoin))&(chmtemp[counterch]>0)){
text(3,7.75,cchmonwords[1],cex=l3)
text(3,7.25,paste(chmtemp[counterch],coin),cex=l3)
money[player]=money[player]+chmtemp[counterch]
}
if(chmtemp[counterch]==(-1000*factorcoin)){
text(3,7.75,paste(cchmonwords[2],2.5*factorcoin,coin),cex=l3)
text(3,7.25,cchmonwords[3],cex=l3)
money[player]=money[player]-2.5*factorcoin
money[-player]=money[-player]+2.5*factorcoin
}
if(chmtemp[counterch]==(-1001*factorcoin)){
text(3,7.75,paste(cchmonwords[1],2.5*factorcoin,coin,cchmonwords[4]),cex=l3)
text(3,7.25,paste(10*factorcoin,coin,cchmonwords[5]),cex=l3)
moneyhousehotel=houses[property==player]
moneyhousehotel[moneyhousehotel==5]=4
money[player]=money[player]-sum(moneyhousehotel*2.5)
}
if(chmtemp[counterch]==(-1002*factorcoin)){
text(3,7.75,paste(cchmonwords[1],5*factorcoin,coin),cex=l3)
text(3,7.25,cchmonwords[3],cex=l3)
money[player]=money[player]+5*factorcoin
money[-player]=money[-player]-5*factorcoin
}


if((chtemp[counterch]==0)&(chmtemp[counterch]==0)){
text(3,8.75,chwordtemp1[4],cex=l3)
text(3,8.25,chwordtemp2[4],cex=l3)
}

if(counterch==16){
#Shuffle
chpos=sample(1:16,size=16,replace=FALSE)
ch=ch[chpos]
chm=chm[chpos]
counterch=0
#Remove the jail card
if(sum(chjail)>0){chused=2}
}

if((counterch>=15)&(chused==2)){
#Shuffle
chpos2=sample(1:15,size=15,replace=FALSE)
ch2=ch2[chpos2]
chm2=chm2[chpos2]
counterch=0
#Add the jail card
if(sum(chjail)==0){chused=1}
}


}


if(!posit%in%c(3,18,34)){
x=c(7,7,9,9)
y=c(2,4,4,2)
polygon(x,y,col="white")
text(8,3.5,toupper(wordtemp1[3]),cex=l2)
text(8,2.5,toupper(wordtemp2[3]),cex=l2)
}else{

x=c(7,7,9,9)
y=c(2,4,4,2)
polygon(x,y,col="white")

countercc=countercc+1

if(ccused==1){cctemp=cc}
if(ccused==2){cctemp=cc2}
if(ccused==1){ccmtemp=ccm}
if(ccused==2){ccmtemp=ccm2}

if(cctemp[countercc] == -2000){
ccjail[player]=1
text(3,8.75,ccwordtemp1[3],cex=l3)
text(3,8.25,ccwordtemp2[3],cex=l3)
}

if(cctemp[countercc] >0){
positions[player]=cctemp[countercc] 
if(cctemp[countercc]==31){
text(8,3.75,ccwordtemp1[1],cex=l3)
text(8,3.25,ccwordtemp2[1],cex=l3)
}else{
text(8,3.75,ccwordtemp1[2],cex=l3)
text(8,3.25,paste(ccwordtemp2[2],cctemp[countercc]),cex=l3)
}
}


if((ccmtemp[countercc]>(-100*factorcoin))&(ccmtemp[countercc]<0)){
text(8,2.75,cchmonwords[2],cex=l3)
text(8,2.25,paste(abs(ccmtemp[countercc]),coin),cex=l3)
money[player]=money[player]+ccmtemp[countercc]
}
if((ccmtemp[countercc]>(-100*factorcoin))&(ccmtemp[countercc]>0)){
text(8,2.75,cchmonwords[1],cex=l3)
text(8,2.25,paste(ccmtemp[countercc],coin),cex=l3)
money[player]=money[player]+ccmtemp[countercc]
}
if(ccmtemp[countercc]==(-1000*factorcoin)){
text(8,2.75,paste(cchmonwords[2],2.5*factorcoin,coin),cex=l3)
text(8,2.25,cchmonwords[3],cex=l3)
money[player]=money[player]-2.5*factorcoin
money[-player]=money[-player]+2.5*factorcoin
}
if(ccmtemp[countercc]==(-1001*factorcoin)){
text(8,2.75,paste(cchmonwords[1],2.5*factorcoin,coin,cchmonwords[4]),cex=l3)
text(8,2.25,paste(10*factorcoin,coin,cchmonwords[5]),cex=l3)
moneyhousehotel=houses[property==player]
moneyhousehotel[moneyhousehotel==5]=4
money[player]=money[player]-sum(moneyhousehotel*2.5)
}
if(ccmtemp[countercc]==(-1002*factorcoin)){
text(8,2.75,paste(cchmonwords[1],5*factorcoin,coin),cex=l3)
text(8,2.25,cchmonwords[3],cex=l3)
money[player]=money[player]+5*factorcoin
money[-player]=money[-player]-5*factorcoin
}

if((cctemp[countercc]==0)&(ccmtemp[countercc]==0)){
text(8,3.75,chwordtemp1[4],cex=l3)
text(8,3.25,chwordtemp2[4],cex=l3)
}

if(countercc==16){
#Shuffle
ccpos=sample(1:16,size=16,replace=FALSE)
cc=cc[ccpos]
ccm=ccm[ccpos]
countercc=0
#Remove the jail card
if(sum(ccjail)>0){ccused=2}
}

if((countercc>=15)&(ccused==2)){
#Shuffle
ccpos2=sample(1:15,size=15,replace=FALSE)
cc2=cc2[ccpos2]
ccm2=ccm2[ccpos2]
countercc=0
#Add the jail card
if(sum(ccjail)==0){ccused=1}
}


}

return(list(ch,cc,chm,ccm,ch2,cc2,chm2,ccm2,counterch,countercc,positions,money,chjail,ccjail,chused,ccused))


}

