### Libraries ----

library(pdftools)

### Convert .pdf to .jpeg

# Single file

pdf_convert(pdf = paste0(getwd(), "/_maps/_fmz_surfgeo__DDP_5.pdf"), format = 'jpeg', page = 1, filename = '_test.jpg', dpi = 300)

# Entire directory

# The following can only be run once without complications associated with
# creating and changeing working directories.  If you screw this up, you're
# smart enough to figure it out...

directory <- paste0(getwd(), '/_maps')

file.list <- paste0(directory, "/", list.files(directory, pattern = "*.pdf"))

file.list

dir.create('_jpeg')

setwd('_jpeg')
getwd()

lapply(file.list, FUN = function(files){
  pdf_convert(files, format = "jpeg", dpi = 300)
})

setwd('..')
getwd()
