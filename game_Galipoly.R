
#Where the files are
folder="C:/Users/Usuario/Documents/Galipoly/"

#Dimensions of the window
width.window=20
height.window=12

#Script with the name of the streets
foldernam=paste(folder,"names_Galipoly.R",sep="")
source(foldernam)

#Coins
coins=c("ptas.","euros","dollars","pounds")
#The money converter
factorcoins=c(1000,10,10,10)

############################################################

#Graphic interface
library("tcltk2")


#Script with the board and generic things
foldergen=paste(folder,"generic_Galipoly.R",sep="")

#Script with the windows (graphical interface)
foldergrapint=paste(folder,"graphical_interface_Galipoly.R",sep="")
source(foldergrapint)

####################################################################
####################################################################
####################################################################


#Script with the language
languagesavail=c("Castellano","Galego","English")
languageChoice=GI1()
if(languageChoice==1){
folderlan=paste(folder,"laguage_es_Galipoly.R",sep="")}
if(languageChoice==2){
folderlan=paste(folder,"laguage_gl_Galipoly.R",sep="")}
if(languageChoice==3){
folderlan=paste(folder,"laguage_en_Galipoly.R",sep="")}
source(folderlan)
#Number of players
nplayers=GI2()
#Name of players
namep=GI3()

coininfo=GI17()
coin=coininfo[1]
factorcoin=as.double(coininfo[2])

source(foldergen)



##########################################################

#Let's start the game
player=1
endofgame=0


while(endofgame==0){
source(folderplay)
}
