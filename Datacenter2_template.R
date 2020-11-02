library(plotly)
library(shiny)
library(readxl)
library(shinythemes)
library(dplyr)
library(lubridate)
library(DT)

ui <- navbarPage("trushrimp data center", theme = shinytheme("flatly"),fluid = TRUE,
                 tabPanel("Reef",h2("REEF"),
                          sidebarLayout(
                            sidebarPanel(width=2,
                                         h4("Table1"),
                                         conditionalPanel(condition="input.tabselected==1",
                                                          uiOutput("choose_dataset"),
                                                          uiOutput("choose_columns"),
                                                          br()),
                                         conditionalPanel(condition="input.tabselected==2",
                                                          uiOutput("varx"),
                                                          uiOutput("vary"),
                                                          uiOutput("varz"),
                                                          br()),
                                         h4("Table2"),
                                         conditionalPanel(condition="input.tabselected2==1",
                                                          uiOutput("choose_dataset2"),
                                                          uiOutput("choose_columns2"))
                            ),
                            mainPanel(
                              fluidRow(
                                column(6,tabsetPanel(
                                  tabPanel("Table 1",value=1,DT::dataTableOutput("data_table"),style = "height:950px%;overflow-y: scroll;overflow-x: scroll;"), #verbatimTextOutput
                                  tabPanel("Plot 1",value=2,plotlyOutput("plot")),id = "tabselected")),
                                column(6,tabsetPanel(
                                  tabPanel("Table 2", value=1, DT::dataTableOutput("data_table2"),style = "height:950px;overflow-y: scroll;overflow-x: scroll;"),id = "tabselected2"))
                              )
                            )
                          )),
                 tabPanel("Hatchery", h2("Hatchery")),
                 tabPanel("Innovation Center", h2("Innovation Center")),
                 tabPanel("Analytic Center", h2("Analytic Center"))
)
server <- function(input, output) {
  
  # Select Dataset
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  
  # Select Column 
  output$choose_columns <- renderUI({
    if(is.null(input$dataset))
      return()
    dat <- get(input$dataset)
    colnames <- names(dat)
    selectizeInput("columns", "Select columns", 
                   choices  = colnames,
                   selected = c("Date","Cohort","Tank"),
                   multiple=TRUE)
  })
  # Select Dataset2
  output$choose_dataset2 <- renderUI({
    selectInput("dataset2", "Data set", as.list(data_sets2))
  })
  # Select Column2
  output$choose_columns2 <- renderUI({
    if(is.null(input$dataset2))
      return()
    dat <- get(input$dataset2)
    colnames <- names(dat)
    selectizeInput("columns2", "Select columns",
                   choices  = colnames,
                   selected = c("Date","Cohort","Tank"),
                   multiple=TRUE)
  })
  #################################################################################
  ## Output: datatable 1
  dt <- reactive ({
    dat <- get(input$dataset)
    dat %>% 
      select(input$columns)
  })
  output$data_table <- renderDataTable({ 
    DT::datatable(dt(), extensions = 'Buttons',filter = "top"
                  ,options = list( 
                    dom = "Blfrtip",
                    searching = FALSE,
                    buttons = list("copy", list(extend = "collection"
                                                , buttons = c("csv", "excel", "pdf")
                                                , text = "Download")) 
                    # customize the length menu
                    , lengthMenu = list( c(10, 20,50, -1) # declare values
                                         , c(10, 20,50, "All")) 
                    , pageLength = 50
                  ) # end of options
                  
    ) # end of datatables
  })  
  
  ## Output: datatable 2
  dt2 <- reactive ({
    dat2 <- get(input$dataset2)    
    dat2 %>% 
      select(input$columns2)
  })
  output$data_table2 <- renderDataTable({ 
    DT::datatable(dt2(), extensions = 'Buttons',filter = "top"
                  ,options = list( 
                    dom = "Blfrtip",
                    searching = FALSE,
                    buttons = list("copy", list(extend = "collection"
                                                , buttons = c("csv", "excel", "pdf")
                                                , text = "Download")) 
                    # customize the length menu
                    , lengthMenu = list( c(10, 20,50, -1) # declare values
                                         , c(10, 20,50, "All")) 
                    , pageLength = 50
                  ) # end of options
                  
    ) # end of datatables
  })  
  #################################################################################
  ## Output: plot
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    selectInput("variablex", "select the X variable", choices=names(dt()))
  })
  # Pulling the list of variable for choice of variable y
  output$vary <- renderUI({
    selectInput("variabley", "select the Y variable", choices=names(dt()))
  })
  output$varz <- renderUI({
    selectInput("variablez", "select the Group variable", choices=names(dt()))
  })
  # For plot
  output$plot <- renderPlotly({
    ggplotly(ggplot(dt(),aes_string(x=input$variablex, y=input$variabley,color=input$variablez)) +
               geom_point(size=2) +
               geom_line(data=dt(),aes_string(x=input$variablex, y=input$variabley,group=input$variablez))+
               theme(legend.position = "bottom",
                     axis.text.x = element_text(size = 8, colour = 'black', angle = 90)))
  })
}

shinyApp(ui, server)