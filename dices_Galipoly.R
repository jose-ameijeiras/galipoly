
#Dices (plot and roll)

#Dice roll 
dicefunc=function(player){
d1=sample(1:6,size=1,replace=FALSE)
d2=sample(1:6,size=1,replace=FALSE)
positions[player]=positions[player]+d1+d2
if(positions[player]>40){positions[player]=positions[player]-40}

#Plot Dice 1
x=c(6,6,7,7)
y=c(8,9,9,8)
polygon(x,y,col="white")
if(d1==1){
points(6.5,8.5)
}
if(d1==2){
points(6.75,8.75)
points(6.25,8.25)
}
if(d1==3){
points(6.75,8.75)
points(6.5,8.5)
points(6.25,8.25)
}
if(d1==4){
points(6.75,8.75)
points(6.75,8.25)
points(6.25,8.25)
points(6.25,8.75)
}
if(d1==5){
points(6.5,8.5)
points(6.75,8.75)
points(6.75,8.25)
points(6.25,8.25)
points(6.25,8.75)
}
if(d1==6){
points(6.75,8.5)
points(6.25,8.5)
points(6.75,8.75)
points(6.75,8.25)
points(6.25,8.25)
points(6.25,8.75)
}

#Plot Dice 2
x=c(8,8,9,9)
y=c(8,9,9,8)
polygon(x,y,col="white")
if(d2==1){
points(8.5,8.5)
}
if(d2==2){
points(8.75,8.75)
points(8.25,8.25)
}
if(d2==3){
points(8.75,8.75)
points(8.5,8.5)
points(8.25,8.25)
}
if(d2==4){
points(8.75,8.75)
points(8.75,8.25)
points(8.25,8.25)
points(8.25,8.75)
}
if(d2==5){
points(8.5,8.5)
points(8.75,8.75)
points(8.75,8.25)
points(8.25,8.25)
points(8.25,8.75)
}
if(d2==6){
points(8.75,8.5)
points(8.25,8.5)
points(8.75,8.75)
points(8.75,8.25)
points(8.25,8.25)
points(8.25,8.75)
}

return(list(positions,d1,d2))

}

