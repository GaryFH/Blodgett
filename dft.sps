SET DECIMAL=DOT.

DATA LIST FILE= "c:/Users/gary/Dropbox/Rstudio/R data files/Blodgett/Blodgett/dft2.txt"  free (",")
ENCODING="Locale"
/ WaterSafety * WaterStorageVessel (F8.0) 
  .

VARIABLE LABELS
WaterSafety "WaterSafety" 
 WaterStorageVessel "WaterStorageVessel" 
 .

VALUE LABELS
/
WaterStorageVessel 
1 "Ceramic narrow mouth" 
 2 "Ceramic narrow mouth with spigot" 
 3 "Ceramic wide mouth" 
 4 "Plastic" 
.
VARIABLE LEVEL WaterSafety 
 (scale).

EXECUTE.
