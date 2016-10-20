#Sell (and motgage) properties

#Do you have properties?
if(sum(property==player)>0){

repeatsell=1
#Your properties
totalsell=numeric()
totalmortg=numeric()
for(ik in which(abs(property)==player)){
#if(property[ik]==player){print(paste(ik,allstn[ik],sep=": "))}
#if(property[ik]==-player){print(paste(paste(ik,allstn[ik],sep=": "),msgsell12,sep=" "))}
totalsell=c(totalsell,ik)
totalmortg=c(totalmortg,property[ik]!=player)
}
issell2=GI7()
if(issell2>0){issell2=totalsell[issell2]}

sellaction=0

if(issell2%in%totalsell){
#Your earnings
if(!issell2%in%c(6,13,16,26,29,36)){
sellprice=priceshouse[locationsrent[issell2]]*houses[issell2]
mortageprice=sellprice+pricesmort[locationsrent[issell2]]
removemortprice=1.1*pricesmort[locationsrent[issell2]]
sellprice=sellprice-pricesst[issell2]
}else{
if(issell2%in%c(13,29)){
mortageprice=7.5*factorcoin
}else{
mortageprice=10*factorcoin
}
removemortprice=1.1*mortageprice
sellprice=-pricesst[issell2]
}


issell3=GI8()

if((issell3==1)&(property[issell2]==player)){
property[issell2]=0
houses[issell2]=0
money[player]=money[player]+sellprice
sellaction=1
}
if((issell3==2)&(property[issell2]==player)){
property[issell2]=-property[issell2]
houses[issell2]=0
money[player]=money[player]+mortageprice
sellaction=1
}
if((issell3==3)&(property[issell2]==-player)){
property[issell2]=-property[issell2]
money[player]=money[player]-removemortprice
sellaction=1
}
if((issell3>=4)&(property[issell2]==player)){

issell4=issell3-3
if((issell4<=houses[issell2])&(issell4>0)){
money[player]=money[player]+priceshouse[locationsrent[issell2]]*issell4
houses[issell2]=houses[issell2]-issell4
sellaction=1
}
}


}

if((sellaction==0)){
if(issell2>0){GI9()}
}else{
moneyfunc()
boxfunc(issell2)
}

if(issell2==0){
repeatsell=0
}

}else{
repeatsell=0
}