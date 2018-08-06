library(shiny)
library(babynames)
library(tidyverse)
library(plotly)

###Rename the data

mydata <- babynames

##UI

ui <- fluidPage(
   titlePanel("Baby names"),
   sidebarLayout(
      sidebarPanel(
        radioButtons("gender",label="Choose a gender", choices=c("Male","Female")),
        selectInput("numorprop", choices=c("Number","Proportion"), label="Choose data display (absolute numbers or proportions)"),
         textInput("textname", label="Type in a name (watch your spelling!)", value="Bruce"),
         textInput("comparisonname1", label="Compare with")
      ),
      mainPanel(
        plotlyOutput("namePlot")
      )
   )
)

server <- function(input, output) {
  
mydata1 <- reactive({
  if (input$gender=="Female") {
  mydata %>%
    filter(name==input$textname,
           sex=="F")
  }
  else{
    mydata %>%
      filter(name==input$textname,
             sex=="M")
  }
})

mydata2 <- reactive({
  if (input$gender=="Female") {
  mydata %>%
    filter(name==input$textname | name==input$comparisonname1) %>%
      filter(sex=="F")
  }
  else {
    mydata %>%
      filter(name==input$textname | name==input$comparisonname1) %>%
      filter(sex=="M")
  }
})

##Single baby name only graph

   output$namePlot <- renderPlotly({
     if (input$numorprop=="Proportion" & 
         input$comparisonname1=="") {
     ggplot(mydata1(), aes(year, prop, group=1)) +
       geom_line()
     }
     else{
       if (input$numorprop=="Number" & 
           input$comparisonname1=="") {
       ggplot(mydata1(), aes(year, n, group=1)) +
         geom_line()
       }
       else{
         if (input$comparisonname1!="" & 
             input$numorprop=="Number") {
           ggplot(mydata2(), aes(year, n, group=name)) +
             geom_line(aes(color=name))
         }
         else{
           if (input$comparisonname1!="" & 
               input$numorprop=="Proportion") {
             ggplot(mydata2(), aes(year, prop, group=name)) +
               geom_line(aes(color=name))
           }
           else{NULL}
       }
       }
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

