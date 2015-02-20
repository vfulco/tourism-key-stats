# compile pdf file from knitr ---------------------------------------------
knitr_source_dir <- "knitr/"
knitr_source_fname <- "word.Rnw"
knitr_output <- "outputs/word.pdf"

require(tools)
require(knitr)
project_wd <- getwd()
setwd(file.path(project_wd, knitr_source_dir))
knit2pdf(input=knitr_source_fname, 
         compiler = 'xelatex', 
         quiet=TRUE, 
         clean = TRUE)
file.copy(gsub(".Rnw", ".pdf", knitr_source_fname, fixed=TRUE), 
          file.path(project_wd, knitr_output), overwrite = TRUE)
setwd(project_wd)

# # clean knitr source folder -----------------------------------------------
# files2clean <- setdiff(list.files(path=knitr_source_dir, all.files = TRUE),  
                       # c(".", "..", 
                         # list.files(path=knitr_source_dir, pattern="\\.bib"), 
                         # list.files(path=knitr_source_dir, pattern="\\.Rnw"), 
                         # list.files(path=knitr_source_dir, pattern="\\.rnw"), 
                         # list.files(path=knitr_source_dir, pattern="\\.pdf"))
# )
# if(length(files2clean)>0) {
  # unlink(paste0(knitr_source_dir, files2clean), recursive =TRUE)
# }
