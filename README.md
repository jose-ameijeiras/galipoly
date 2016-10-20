# galipoly
This is the boarding game Galipoly

1-To play the game

        1a-Download all the R files in the same folder. 

	1b- Open game_Galipoly.R
  
	1c- Change in the file the folder direction for where the Complete_Galipoly is downloaded.
  
	1d- Execute this script
  
2-To change the street names: names_Galipoly.R

3-To add a new language

	3a-Copy the style of laguage_es_Galipoly.R and create a new script with your desired language
  
	3b-Name it as laguage_xx_Galipoly.R (where xx is your language)
  
	3c-In game_Galipoly.R add your language to the vector "languagesavail" after the current ones
  
	3d-After the conditionals "languageChoice" add the folder of your language
  
	   If there was n languages available. Then do it in the following way
		if(languageChoice== (n+1) ){
		folderlan=paste(folder,"laguage_xx_Galipoly.R",sep="")}
    
4-To add a new currency

	4a-In game_Galipoly.R add your currency to the vector "coins" after the current ones.
  
	4b-Introduce the converter rate in the vector "factorcoins" after the current ones.
