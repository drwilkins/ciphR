#by Matt Wilkins (mattwilkinsbio.com)
#emoji object taken from https://github.com/hadley/emo 
library(shiny);require(ggplot2);require(tibble);require(DT)
load(file="emojis.rda",verbose=T)

#########################################################
# Define UI for application that draws a histogram
ui <- fluidPage(theme="bootstrap.css",
   
   # Application title
   titlePanel("CiphR: secret codes for education"),
  #withTags()
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textAreaInput("string","Text to be encoded",value=""), 
        numericInput("shift","Shift letters? (+/- integer)",value=0),
        checkboxInput("glyphs","Random Glyphs instead of A-Z?",value=F),
        conditionalPanel('input.glyphs==true',
          uiOutput("seed")),
        sliderInput("fontscale","Font Size",value=22,min=10,max=60)#,
        #submitButton("Submit",icon=icon("laptop-code"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        #tags$body(class='thead',style="display:none"),
         #plotOutput("distPlot"),
        conditionalPanel('input.string !=""',
          fluidRow(
          column(width=12,align="left",
          h1("coded_msg>> "),
          uiOutput("newmsg"),
          br(),
          h1("key>> "),
          span(textOutput("warn"),style='color:red'),
          uiOutput("DToutput"),
          tags$br()
          ))
        )#end Conditional Panel
      )
   )
)


###############################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
  d<-reactiveValues() 
  
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
#render the coded message
   output$coded<-renderText({
     x<-input$string
     x.vec<-tolower(unlist(strsplit(x,fixed=T,split="")))#all lower case as vector
     
     #define new alphabet indexes for key
      alphabet<-1:26+input$shift
      alphabet.shifted<-sapply(alphabet,function(x) {if(x>26){x-26}else{ if(x<1){x+26}else{x}}})
      #assign reactive value (global) variable w/ shifted alphabet 
      d$alphabet.out<-letters[alphabet.shifted]
     
     #shift message according to user input
     if(input$shift!=0){x.vec<-
       sapply(x.vec,function(s) {
         if(!s%in%letters){s}else{#If nonletter, leave it alone, else...
         shifted<-match(s,letters)+input$shift
         ifelse(shifted>26,shifted<- shifted-26,ifelse(shifted<1,shifted<-shifted+26,""))
         letters[shifted]
         }
      })
     }#Only do any of this if shift not 0
         
      #if user wants glyphs, assign emojis to each letter
      if(input$glyphs==T){
        #isolate({
          set.seed(input$seedval)
          glyphabet.indx<-sample(1:nrow(jis),26,replace=F)
        #})
        glyphabet<-jis$emoji[glyphabet.indx]
        d$alphabet.out<-glyphabet
        #reassign x.vec (which may have already been shifted) to emojis
        x.vec<-sapply(x.vec,function(s) {
         if(!s%in%letters){s}else{#If nonletter, leave it alone, else...
         alpha.indx<-match(s,letters)
         emojied<-glyphabet[alpha.indx]
         emojied
         }})
      }
     
      #%%%% Output the secret message
       newmsg<-paste0(x.vec,collapse="") #new (coded) message vector
         #Output new message if new string isn't blank
        ifelse(x=="","",newmsg) 
       })
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
   #Output Key table
    output$key<-renderDataTable({
    tbl<-tibble(input=letters,output=d$alphabet.out)
    widetbl<-t(tbl)
    #setNames(widetbl,rep("",13))#paste0("V",1:26)
    widetbl<-rbind(widetbl[,1:13],widetbl[,14:26])
    datatable(widetbl,colnames=rep("",13),options=list(paging=F,searching=F,ordering=F,info=F,scrollX=T,autoWidth=T))
   # formatStyle(table=datatable(widetbl),columns=paste0("V",1:26),backgroundColor = "yellow")
    })#,columnDefs = list(list(width = 1, targets = "_all"))))
   
    
    #warning output
    output$warn<-renderText({
      if(input$shift!=0 & input$glyphs==T){
        paste0("**Note that emojis are shifted ",sprintf ("%+-d", input$shift)," letters")
      }
    })
    
    #Output or take input from user for replicable glyph alphabet sets
    output$seed<-renderUI({
      if(input$glyphs==T){
        numericInput("seedval","Glyph Set (random by default):",value=sample.int(1000,1),min=1,max=nrow(jis))
      }
    })
    
    output$newmsg<-renderUI({
      div(HTML(paste0("<pre>",textOutput("coded"),"</pre>")),class="outputfield")
    })
    
    output$DToutput<-renderUI({
      span(dataTableOutput("key"),style=paste0('float: left; font-size:', input$fontscale,'px'))
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

