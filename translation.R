translation <- function(ndir){

if(ndir == "North"){
  tdir <- "South"
}
if(ndir == "South"){
  tdir <- "North"
}
if(ndir == "East"){
  tdir <- "West"
}
if(ndir == "West"){
  tdir <- "East"
}
if(ndir == "North West"){
  tdir <- "South East"
}
if(ndir == "South East"){
  tdir <- "North West"
}
if(ndir == "North East"){
  tdir <- "South West"
}
if(ndir == "South West"){
  tdir <- "North East"
}
  return(tdir)
}