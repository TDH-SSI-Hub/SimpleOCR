
library('png')
library('shiny')
library('plotly')
library('magick')
library('tesseract')
library('formattable')
#library('rstudioapi')

#folder<-dirname(rstudioapi::getSourceEditorContext()$path)
#setwd(folder)

base.code<-readLines('OCR template.r')


get.value<-function(element){
    if(is.na(element)){
        "NA"
    }else{paste0("'",element,"'")}
}
get.string<-function(object){
paste(sapply(object,get.value),collapse = ',')
}



get.string(c(NA,NA,NA,NA))

lab.elements<-c('Name','DOB','Address','Address2','Spec.Col.Dt','Accession','Test','Result')
space<-' '
lowercase<-'abcdefghijklmnopqrstuvwxyz'
uppercase<-toupper(lowercase)
numbers<-'0123456789'
punctuation<-':()/-.,'
engines<-list(Spaces=space,Lowercase=lowercase,Uppercase=uppercase,Numbers=numbers,Punctuation=punctuation)

read.element<-function(image,geometry,engine){
    if (!is.na(geometry)){
        if (!is.na(engine)){
            engine1<-tesseract(options = list(tessedit_char_whitelist = engine))
        }else{engine1<-'eng'}
        gsub('\n','', ocr(image_crop(image,geometry),engine = engine1))
    }else{NA}
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("OCR generator"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 4
                     ,selectInput('file','File',choices = 'Example PDF')
                     ,h4("Drag a box around a page element to the right and select 'Read' or 'Add to Report'")
                     ,selectInput('colname','Column Name',choices = lab.elements)
                     ,checkboxGroupInput('engine','Allowed Characters (optional, blank is all characters)',  choices = engines)
                     ,actionButton('Read','Read')
                     ,actionButton('Add','Add to Report')
                     ,textOutput('brushed')
                     ,formattableOutput('report')
                     ,h4('File creation functionality disabled. In real use, this section would create a script that could process similar files in the future.')
                     ,textInput('filename','File Name', value = 'Generated OCR code')
                     ,actionButton('Code','Generate Code')
                     ,actionButton('Run','Run Code')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(width = 8
                  ,plotlyOutput("distPlot", height = '1100px',width = '850px' )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    values<-reactiveValues()
    values$test<-image_read_pdf('example1.pdf', density = 400)
    #observe(values$test<-image_read_pdf(input$file))
    
    observe(values$test.raster<-image_convert(values$test,format = 'raster'))
    observe(values$test.info<-image_info(values$test))
    observe(values$img.width<-as.integer(values$test.info[2]))
    observe(values$img.height<-as.integer(values$test.info[3]))
    observe(values$test.df<-data.frame(x=c(0,values$img.width),y=c(0,values$img.height)))
    
    
    
    values$report<-data.frame(colname=lab.elements,crop=NA,engine=NA,value=NA)
    output$report<-renderFormattable(formattable(values$report[,c(1,4)]))
    
    observeEvent(event_data("plotly_brushed"),{
        values$xmin<-{
            xmin<-event_data("plotly_brushed")$x[1]
            if(xmin<0) xmin<-0
            round(xmin,0)
        }
        values$xmax<-{
            xmax<-event_data("plotly_brushed")$x[2]
            if(xmax>values$img.width) xmax<-values$img.width
            round(xmax,0)
        }
        values$ymin<-{
            ymin<-event_data("plotly_brushed")$y[1]
            if(ymin<0) ymin<-0
            round(ymin,0)
        }
        values$ymax<-{
            ymax<-event_data("plotly_brushed")$y[2]
            if(ymax>values$img.height) ymax<-values$img.height
            round(ymax,0)
        }
        
        values$read.area<-{paste0(values$xmax-values$xmin,'x',values$ymax-values$ymin,'+',values$xmin,'+',values$ymin)}
    })
    
    observeEvent(input$Read,{
        values$ocr<-read.element(values$test,values$read.area,paste(input$engine,collapse = ''))
    })
    
    observeEvent(input$Add,{
        values$report[values$report$colname==input$colname,2:4]<-c(values$read.area,paste(input$engine,collapse = ''),read.element(values$test,values$read.area,paste(input$engine,collapse = '')))
    })
    
    observeEvent(input$Code,{
        base.code[1]<-paste0("column.names<-c(",get.string(values$report$colname),")")
        base.code[2]<-paste0("geometries<-c(",get.string(values$report$crop),")")
        base.code[3]<-paste0("engines<-c(",get.string(values$report$engine),")")
        file.create(paste0(input$filename,'.r'))
        file.chosen<-file(paste0(input$filename,'.r'))
        writeLines(base.code,file.chosen)
    })
    
    observeEvent(input$Run,{
        if(file.exists(paste0(input$filename,'.r'))){
          message('Running Code')
            source(paste0(input$filename,'.r'))
        }
    })
    
    output$brushed <- renderText({ values$ocr})
    
    output$distPlot <- renderPlotly({
        plot_ly(x = values$test.df$x, y = values$test.df$y, type = 'scatter', mode = 'markers') %>%
            layout(
                images = list(
                    list(
                        source =  raster2uri(values$test.raster),
                        xref = "x",
                        yref = "y",
                        x = 0,
                        y = 0,
                        sizex = values$img.width,
                        sizey = values$img.height,
                        sizing = "stretch",
                        opacity = 1,
                        layer = "above"
                    )
                )
            ) %>%  
            layout(dragmode = "select",
                   yaxis = list(autorange = "reversed",
                                title = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE),
                   xaxis = list(
                                title = "",
                                zeroline = FALSE,
                                showline = FALSE,
                                showticklabels = FALSE,
                                showgrid = FALSE)
                   ) %>% config(displayModeBar = F) %>% 
            event_register("plotly_brushed") 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
