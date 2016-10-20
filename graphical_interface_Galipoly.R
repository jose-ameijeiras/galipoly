
#########################################################

#Select the language

GI1=function(){
tt<-tktoplevel()
scr <- tkscrollbar(tt, repeatinterval=5,
				   command=function(...)tkyview(tl,...))
tl<-tklistbox(tt,height=3,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt,text="Select the language"))
tkgrid(tl,scr)
tkgrid.configure(scr,rowspan=20,sticky="nsw")
for(posl in 1:length(languagesavail)){
tkinsert(tl,"end",languagesavail[posl])
}
tkselection.set(tl,0)  # Default is spanish.  Indexing starts at zero.
OnOK <- function()
{
    languageChoice <- as.numeric(tkcurselection(tl))+1
    e <- parent.env(environment())
    e$languageChoice=languageChoice 
    tkdestroy(tt)
}
OK.but <-tkbutton(tt,text="   OK   ",command=OnOK)
tkgrid(OK.but)
tkwait.window(tt)
return(languageChoice)
}


####################################################################

#Select the number of players


GI2=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl2,...))
tl2<-tklistbox(tt2,height=7,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgnumberpl))
tkgrid(tl2,scr)
tkgrid.configure(scr,rowspan=20,sticky="nsw")
for(npl in 2:8){
tkinsert(tl2,"end",npl)
}
tkselection.set(tl2,0)  # Default is 2.  Indexing starts at zero.
OnOK2 <- function()
{
    nplayers<- as.numeric(tkcurselection(tl2))+2
    e <- parent.env(environment())
    e$nplayers=nplayers
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(nplayers)
}

####################################################################

#Do you want to introduce the name of the players


GI3 <- function(){

   x1var <- tclVar(paste(namegen,1,sep=" "))
   x2var <- tclVar(paste(namegen,2,sep=" "))
   x3var <- tclVar(paste(namegen,3,sep=" "))
   x4var <- tclVar(paste(namegen,4,sep=" "))
   x5var <- tclVar(paste(namegen,5,sep=" "))
   x6var <- tclVar(paste(namegen,6,sep=" "))
   x7var <- tclVar(paste(namegen,7,sep=" "))
   x8var <- tclVar(paste(namegen,8,sep=" "))


   tt <- tktoplevel()
   x1.entry <- tkentry(tt, textvariable=x1var)
   x2.entry <- tkentry(tt, textvariable=x2var)
   x3.entry <- tkentry(tt, textvariable=x3var)
   x4.entry <- tkentry(tt, textvariable=x4var)
   x5.entry <- tkentry(tt, textvariable=x5var)
   x6.entry <- tkentry(tt, textvariable=x6var)
   x7.entry <- tkentry(tt, textvariable=x7var)
   x8.entry <- tkentry(tt, textvariable=x8var)



   submit <- function() {
     x1 <- as.character(tclvalue(x1var))
     x2 <- as.character(tclvalue(x2var))
     x3 <- as.character(tclvalue(x3var))
     x4 <- as.character(tclvalue(x4var))
     x5 <- as.character(tclvalue(x5var))
     x6 <- as.character(tclvalue(x6var))
     x7 <- as.character(tclvalue(x7var))
     x8 <- as.character(tclvalue(x8var))

     e <- parent.env(environment())
     e$x1 <- x1
     e$x2 <- x2
     e$x3 <- x3
     e$x4 <- x4
     e$x5 <- x5
     e$x6 <- x6
     e$x7 <- x7
     e$x8 <- x8
     tkdestroy(tt)
   }
   submit.but <- tkbutton(tt, text="   OK   ", command=submit)

   tkgrid(tklabel(tt,text=msgnamepl),columnspan=2)
   tkgrid(tklabel(tt,text=paste(namegen,1,sep=" ")), x1.entry, pady = 10, padx =10)
   tkgrid(tklabel(tt,text=paste(namegen,2,sep=" ")), x2.entry, pady = 10, padx =10)
   if(nplayers>2){
   tkgrid(tklabel(tt,text=paste(namegen,3,sep=" ")), x3.entry, pady = 10, padx =10)}
   if(nplayers>3){
   tkgrid(tklabel(tt,text=paste(namegen,4,sep=" ")), x4.entry, pady = 10, padx =10)}
   if(nplayers>4){
   tkgrid(tklabel(tt,text=paste(namegen,5,sep=" ")), x5.entry, pady = 10, padx =10)}
   if(nplayers>5){
   tkgrid(tklabel(tt,text=paste(namegen,6,sep=" ")), x6.entry, pady = 10, padx =10)}
   if(nplayers>6){
   tkgrid(tklabel(tt,text=paste(namegen,7,sep=" ")), x7.entry, pady = 10, padx =10)}
   if(nplayers>7){
   tkgrid(tklabel(tt,text=paste(namegen,8,sep=" ")), x8.entry, pady = 10, padx =10)}
  
   tkgrid(submit.but)

  tkwait.window(tt)
  namep=c(x1,x2)
  if(nplayers>2){namep=c(namep,x3)}
  if(nplayers>3){namep=c(namep,x4)}
  if(nplayers>4){namep=c(namep,x5)}
  if(nplayers>5){namep=c(namep,x6)}
  if(nplayers>6){namep=c(namep,x7)}
  if(nplayers>7){namep=c(namep,x8)}
  return(namep)
}


####################################################################

#Who is going to play


GI4=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=paste(namep[player],msgdice1)))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}

####################################################################

#Building


GI5=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl3,...))
tl3<-tklistbox(tt2,height=7,width=max(nchar(c(msgbuild1,allstn))),selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgbuild2))
tkgrid(tl3,scr)
tkgrid.configure(scr,rowspan=30,sticky="nsw")
tkinsert(tl3,"end",msgbuild1)
for(npl in totalmonop3){
tkinsert(tl3,"end",allstn[npl])
}
tkselection.set(tl3,0)  # Default is no buildings.  Indexing starts at zero.
OnOK2 <- function()
{
    isbuilt1<- as.numeric(tkcurselection(tl3))
    e <- parent.env(environment())
    e$isbuilt1=isbuilt1
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(isbuilt1)
}

####################################################################

#Number of houses

GI6=function(){
if(houses[isbuilt1]==1){
msgbuildtemp1=(paste(allstn[isbuilt1],msgbuild3,houses[isbuilt1],msgbuild4,sep=" "))
}else{
if(houses[isbuilt1]==5){
msgbuildtemp1=(paste(allstn[isbuilt1],msgbuild3,1,msgbuild6,sep=" "))
}else{
msgbuildtemp1=(paste(allstn[isbuilt1],msgbuild3,houses[isbuilt1],msgbuild5,sep=" "))
}}
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl3,...))
tl3<-tklistbox(tt2,height=7,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgbuildtemp1))
tkgrid(tklabel(tt2,text=msgbuild7))
tkgrid(tl3,scr)
tkgrid.configure(scr,rowspan=30,sticky="nsw")
tkinsert(tl3,"end",msgbuild1)
if(houses[isbuilt1]==0){
tkinsert(tl3,"end",paste(1,msgbuild4,sep=" "))}
if(houses[isbuilt1]<2){
tkinsert(tl3,"end",paste(2,msgbuild5,sep=" "))}
if(houses[isbuilt1]<3){
tkinsert(tl3,"end",paste(3,msgbuild5,sep=" "))}
if(houses[isbuilt1]<4){
tkinsert(tl3,"end",paste(4,msgbuild5,sep=" "))}
if(houses[isbuilt1]<5){
tkinsert(tl3,"end",paste(1,msgbuild6,sep=" "))}


tkselection.set(tl3,0)  # Default is no buildings.  Indexing starts at zero.
OnOK2 <- function()
{
    isbuilt2<- as.numeric(tkcurselection(tl3))
    e <- parent.env(environment())
    e$isbuilt2=isbuilt2
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(isbuilt2)
}

####################################################################

#Selling (select the property)


GI7=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2,repeatinterval=5,
				   command=function(...)tkyview(tl3,...))
tl3<-tklistbox(tt2,height=7, width=max(nchar(c(msgsell2,allstn))),selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgsell1))
tkgrid(tl3,scr)
tkgrid.configure(scr,sticky="nsw")
tkinsert(tl3,"end",msgsell2)
npl2=0
for(npl in totalsell){
npl2=npl2+1
if(totalmortg[npl2]==0){
tkinsert(tl3,"end",allstn[npl])
}else{
tkinsert(tl3,"end",paste(allstn[npl],msgsell12))
}
}
tkselection.set(tl3,0)  # Default is no sellings.  Indexing starts at zero.
OnOK2 <- function()
{
    issell2<- as.numeric(tkcurselection(tl3))
    e <- parent.env(environment())
    e$issell2=issell2
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(issell2)
}


####################################################################

#Selling (choose the option)

GI8=function(){

tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl3,...))
tl3<-tklistbox(tt2,height=9,width=30,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)

if(property[issell2]==player){
tkgrid(tklabel(tt2,text=paste(msgsell4,sellprice,coin,msgsell5,sep=" ")))
tkgrid(tklabel(tt2,text=paste(msgsell4,mortageprice,coin,msgsell6,sep=" ")))
if(houses[issell2]>0){
tkgrid(tklabel(tt2,text=paste(msgsell4,priceshouse[locationsrent[issell2]],coin,msgsell9,5*priceshouse[locationsrent[issell2]],coin,msgsell10,sep=" ")))
if(houses[issell2]==1){
tkgrid(tklabel(tt2,text=paste(msgsell15,houses[issell2],msgsell16,sep=" ")))
}else{
if(houses[issell2]==5){
tkgrid(tklabel(tt2,text=paste(msgsell15,1,msgsell18,sep=" ")))
}else{
tkgrid(tklabel(tt2,text=paste(msgsell15,houses[issell2],msgsell17,sep=" ")))
}
}


}
}else{
tkgrid(tklabel(tt2,text=paste(msgsell7,removemortprice,coin,msgsell8,sep=" ")))
}


tkgrid(tl3,scr)
tkgrid.configure(scr,rowspan=30,sticky="nsw")
tkinsert(tl3,"end",msgsell2)

tkinsert(tl3,"end",msgsell19)
tkinsert(tl3,"end",msgsell20)
tkinsert(tl3,"end",msgsell21)


if(houses[issell2]>0){
tkinsert(tl3,"end",paste(msgsell13,1,msgbuild4,sep=" "))}
if(houses[issell2]>=2){
tkinsert(tl3,"end",paste(msgsell13,2,msgbuild5,sep=" "))}
if(houses[issell2]>=3){
tkinsert(tl3,"end",paste(msgsell13,3,msgbuild5,sep=" "))}
if(houses[issell2]>=4){
tkinsert(tl3,"end",paste(msgsell13,4,msgbuild5,sep=" "))}
if(houses[issell2]==5){
tkinsert(tl3,"end",paste(msgsell13,1,msgbuild6,sep=" "))}


tkselection.set(tl3,0)  # Default is no buildings.  Indexing starts at zero.
OnOK2 <- function()
{
    issell3<- as.numeric(tkcurselection(tl3))
    e <- parent.env(environment())
    e$issell3=issell3
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(issell3)
}

#######################################################

#Selling (Wrong action)


GI9=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=msgsell14))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}


####################################################################

#Jail

GI10=function(){

tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl3,...))
tl3<-tklistbox(tt2,height=3,width=50,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)

tkgrid(tklabel(tt2,text=msgjail1))

tkgrid(tl3,scr)
tkgrid.configure(scr,rowspan=30,sticky="nsw")
tkinsert(tl3,"end",msgjail2)
tkinsert(tl3,"end",msgjail3)

if(chjail[player]==1){
tkinsert(tl3,"end",msgjail4)}
if(ccjail[player]==1){
tkinsert(tl3,"end",msgjail5)}


tkselection.set(tl3,0)  # Default is no buildings.  Indexing starts at zero.
OnOK2 <- function()
{
    jailfree<- as.numeric(tkcurselection(tl3))
    e <- parent.env(environment())
    e$jailfree=jailfree
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(jailfree)
}


####################################################################

#Buy properties


GI11=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl2,...))
tl2<-tklistbox(tt2,height=7,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
msgtobuytemp1=paste(msgtobuy1,allstn[(positions[player])],msgtobuy2,sep="")
tkgrid(tklabel(tt2,text=msgtobuytemp1))
tkgrid(tl2,scr)
tkgrid.configure(scr,rowspan=20,sticky="nsw")

tkinsert(tl2,"end",msgtobuy3)
tkinsert(tl2,"end",msgtobuy4)

tkselection.set(tl2,0)  # Default is 2.  Indexing starts at zero.
OnOK2 <- function()
{
    decission<- as.numeric(tkcurselection(tl2))
    e <- parent.env(environment())
    e$decission=decission
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(decission)
}


#######################################################

#Doubles (another turn)


GI12=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=msgdoubles1))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}

#######################################################

#Doubles (jail)


GI13=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=msgdoubles2))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}

####################################################################

#End the game


GI14=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl2,...))
tl2<-tklistbox(tt2,height=7,width=40,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgend1))
tkgrid(tl2,scr)
tkgrid.configure(scr,rowspan=20,sticky="nsw")

tkinsert(tl2,"end",msgend2)
tkinsert(tl2,"end",msgend3)
tkinsert(tl2,"end",msgend4)

tkselection.set(tl2,0)  # Default is 2.  Indexing starts at zero.
OnOK2 <- function()
{
    endit<- as.numeric(tkcurselection(tl2))
    e <- parent.env(environment())
    e$endit=endit
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(endit)
}

#######################################################

#End of game


GI15=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=paste(namep[player],msgend5,sep=" ")))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}

#######################################################

#Player out


GI16=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=paste(namep[player],msgend6,sep=" ")))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}



####################################################################

#Select the coin


GI17=function(){
tt2<-tktoplevel()
scr <- tkscrollbar(tt2, repeatinterval=5,
				   command=function(...)tkyview(tl2,...))
tl2<-tklistbox(tt2,height=7,selectmode="single",yscrollcommand=function(...)tkset(scr,...),background="white", exportselection=0)
tkgrid(tklabel(tt2,text=msgcoin))
tkgrid(tl2,scr)
tkgrid.configure(scr,rowspan=20,sticky="nsw")

for(npl in 1:length(coins)){
tkinsert(tl2,"end",coins[npl])
}


tkselection.set(tl2,0)  # Default is 2.  Indexing starts at zero.
OnOK2 <- function()
{
    selectedc<-as.numeric(tkcurselection(tl2))+1
    coin<- coins[selectedc]
    factorcoin<-factorcoins[selectedc]
    e <- parent.env(environment())
    e$coin=coin
    e$factorcoin=factorcoin
    tkdestroy(tt2)
}
OK.but2 <-tkbutton(tt2,text="   OK   ",command=OnOK2)
tkgrid(OK.but2)
tkwait.window(tt2)
return(c(coin,factorcoin))
}

#######################################################

#Money warning


GI18=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=paste(namep[player],msgend7,sep=" ")))
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}


#######################################################

#Who won?


GI19=function(){
tt3<-tktoplevel()
tkgrid(tklabel(tt3,text=msgend8))
if(sum(money>0)>0){
tkgrid(tklabel(tt3,text=paste(namep[(money>0)],msgend9,sep=" ")))}
OnOK3 <- function()
{
    tkdestroy(tt3)
}
OK.but2 <-tkbutton(tt3,text="   OK   ",command=OnOK3)
tkgrid(OK.but2)
tkwait.window(tt3)
}

