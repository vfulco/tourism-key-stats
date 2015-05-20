#============ set up R environment ============ #
## customize R package location 
source("P:/r/common.Rprofile")

## makes use of Internet Explorer functions to allow specification of proxies
utils::setInternet2(TRUE)

## get RJSDMX working
Sys.setenv(SDMX_CONF = "P:/R/configuration.properties")
#============ set up R environment ============ #


#============ set up latex environment ============ #
## replace miktex package folder to a customized one
if(file.exists("P:/MiKTeX/libraries/miktex-portable-current")) {
  system("initexmf --register-root=P:/MiKTeX/libraries/miktex-portable-current/")
  system("initexmf -u")
} else if(file.exists(paste0(substr(getwd(),1,2),"/MiKTeX/libraries/miktex-portable-current"))){
  system(paste0("initexmf --register-root=",substr(getwd(),1,2),"/MiKTeX/libraries/miktex-portable-current"))
  system("initexmf -u")
}

## include mbie package 
if(file.exists("P:/MiKTeX/libraries/mbie-template-release")){
  LatexStylesDir <- "P:/MiKTeX/libraries/mbie-template-release/"
} else if(file.exists(paste0(substr(getwd(),1,2),"/MiKTeX/libraries/mbie-template-release"))) {
  LatexStylesDir <- paste0(substr(getwd(),1,2),"/MiKTeX/libraries/mbie-template-release")
}

if(!grepl(LatexStylesDir,Sys.getenv("TEXINPUTS"))){
  Sys.setenv(TEXINPUTS = paste0(LatexStylesDir,";",Sys.getenv("TEXINPUTS")))
} 
Sys.setenv(BIBINPUTS = paste0(getwd(),"/knitr/;",Sys.getenv("BIBINPUTS")))
#============ set up latex environment ============ #
