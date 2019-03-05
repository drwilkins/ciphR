#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);require(ggplot2);require(grid);require(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("CiphR: secret codes for education"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textAreaInput("string","Text to be encoded"), 
        numericInput("shift","Shift letters? (+/- integer)",value=0)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         #plotOutput("distPlot"),
        conditionalPanel('input.string !=""',
          a("coded_msg>> "),
          textOutput("coded"),
            tags$head(tags$style("#coded{font-size: 24px;font-style: bold}")),
          p(),
          a("key>>" ),
          plotOutput("key", height=50)
        )
      )
   )
)

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
   
   output$coded<-renderText({
     x<-input$string
     x.vec<-tolower(unlist(strsplit(x,fixed=T,split="")))#all lower case as vector
     
     #define new alphabet for key
      alphabet<-1:26+input$shift
      alphabet.shifted<-sapply(alphabet,function(x) {if(x>26){x-26}else{ if(x<1){x+26}else{x}}})
      d$alphabet.out<-letters[alphabet.shifted]
     
     
     if(input$shift!=0){x.vec<-
       sapply(x.vec,function(s) {
         if(!s%in%letters){s}else{#If nonletter, leave it alone, else...
         shifted<-match(s,letters)+input$shift
         ifelse(shifted>26,shifted<- shifted-26,ifelse(shifted<1,shifted<-shifted+26,""))
         letters[shifted]
         }
      })
     }#Only do any of this if shift not 0
         
   newmsg<-paste0(x.vec,collapse="")
     
    ifelse(x=="","",paste0("",newmsg)) 
   })

    output$key<-renderPlot({
    tbl<-rbind(input=letters,output=d$alphabet.out)
    grid.table(tbl,theme=ttheme_default(base_size = 18))
    })
   
   
   }


# Run the application 
shinyApp(ui = ui, server = server)

