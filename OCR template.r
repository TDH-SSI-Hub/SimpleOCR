column.names<-c()
geometries<-c()
engines<-c()

library('magick')
library('tesseract')

all.pdfs<-list.files(pattern='.pdf')

create.empty.df<-function(column.names, rows=0){
  base.df<-as.data.frame(matrix(ncol = length(column.names), nrow = rows))
  colnames(base.df)<-column.names
  return(base.df)
}

read.element<-function(image,geometry,engine){
  if (!is.na(geometry)){
    if (!is.na(engine)){
      engine1<-tesseract(options = list(tessedit_char_whitelist = engine))
    }else{engine1<-'eng'}
    gsub('\n','', ocr(image_crop(image,geometry),engine = engine1))
  }else{NA}
}

read.pdf<-function(file){
  newrow<-create.empty.df(column.names,1)
  pdf.image<-image_read_pdf(file, density=400)

  for(c in 1:length(column.names)){
    newrow[1,c]<-read.element(pdf.image,geometries[c], engines[c])
  }
  return(newrow)
}

base.df<-create.empty.df(column.names = column.names)

for (f in all.pdfs){
  base.df<-rbind(base.df,read.pdf(f))
}

write.csv(base.df,'lab_output.csv')






