library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "trushrimp data center"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green")
    )
  ),
  
  dashboardBody(
    fluidRow(
      box(
        title = "Dataset", status = "primary", solidHeader = TRUE, width = 3,
        #"Box content here", br(), "More box content",
        selectInput("TheFile", "Select Dataset", 
                    choices = c("Metadata.csv", "Micro_hatchery.csv"))
      ),
      box(title = "1st Table", status = "primary", solidHeader = TRUE, width = 3,
          selectizeInput('Cohorts', 'Select Cohort:', choices = c(unique(data$Cohort))),
          selectizeInput('Tanks', 'Select Tank:', choices = c( unique(data$Tank))),
          selectizeInput(inputId = 'Parameters', 
                         label = 'Parameters:', 
                         choices = c(colnames(data)), multiple = TRUE),
          sliderInput(inputId = "Tenures", 
                      label = "Tank Tenure Range", min = min(na.omit(data$Tank.Tenure)), 
                      max =  max(na.omit(data$Tank.Tenure)), value = c(40, 60))
      ),
      box(title = "2nd Table", status = "primary", solidHeader = TRUE, width = 3,
          selectizeInput('Cohorts2', 'Select Cohort:', choices = c(unique(data$Cohort))),
          selectizeInput('Tanks2', 'Select Tank:', choices = c( unique(data$Tank)))
      )
    ),
    fluidRow(
      tabBox(
        #title ="Table1",
        id="tabset1",
        tabPanel("Table",DT::dataTableOutput("table"),style = "height:950px;overflow-y: scroll;overflow-x: scroll;"),
        tabPanel("Satistics",verbatimTextOutput("table2"))
      ),
      tabBox(
        tabPanel("Table2",DT::dataTableOutput("table3"),style = "height:950px;overflow-y: scroll;overflow-x: scroll;")
      )
    )
  )
)

server <- function(input, output,session) {
  #read csv file
  myCSV <- reactive({
    read.csv(input$TheFile)
  })
  
  #reactive list(1)
  Tanks.choice <- reactive({
    myCSV() %>% 
      filter(Cohort ==input$Cohorts) %>%
      pull(Tank)
  })
  #observe(1)
  observe({
    updateSelectizeInput(session, "Tanks", choices = Tanks.choice())
  })
  
  #reactive list(2)
  Tanks.choice2 <- reactive({
    myCSV() %>% 
      filter(Cohort ==input$Cohorts2) %>%
      pull(Tank)
  })
  #observe(2)
  observe({
    updateSelectizeInput(session, "Tanks2", choices = Tanks.choice2())
  })
  
  #Datatable(1)
  tab <- reactive({ 
    myCSV() %>% 
      select(input$Parameters) %>%
      filter(Cohort == input$Cohorts) %>% 
      filter(Tank == input$Tanks) %>%
      filter(Tank.Tenure> input$Tenures[1],Tank.Tenure < input$Tenures[2])
    
  })
  output$table <- DT::renderDataTable({ 
    DT::datatable(select(tab(),-c(Cohort)), extensions = 'Buttons'
                  , options = list( 
                    dom = "Blfrtip"
                    , buttons = 
                      list("copy", list(
                        extend = "collection"
                        , buttons = c("csv", "excel", "pdf")
                        , text = "Download"
                      ) ) # end of buttons customization
                    # customize the length menu
                    , lengthMenu = list( c(10, 20, -1) # declare values
                                         , c(10, 20, "All") # declare titles
                    ) # end of lengthMenu customization
                    , pageLength = 20
                    ,autoWidth = TRUE
                  ) # end of options
    ) # end of datatables
  })
  
  #Datatable(2)
  tab2 <- reactive({ 
    myCSV() %>% 
      select(input$Parameters) %>%
      filter(Cohort == input$Cohorts2) %>% 
      filter(Tank == input$Tanks2) %>%
      filter(Tank.Tenure> input$Tenures[1],Tank.Tenure < input$Tenures[2])
    
  })
  output$table3 <-   DT::renderDataTable({ 
    DT::datatable(select(tab2(),-c(Cohort)), extensions = 'Buttons'
                  , options = list( 
                    dom = "Blfrtip"
                    , buttons = 
                      list("copy", list(
                        extend = "collection"
                        , buttons = c("csv", "excel", "pdf")
                        , text = "Download"
                      ) ) # end of buttons customization
                    # customize the length menu
                    , lengthMenu = list( c(10, 20, -1) # declare values
                                         , c(10, 20, "All") # declare titles
                    ) # end of lengthMenu customization
                    , pageLength = 20
                    ,autoWidth = TRUE
                  ) # end of options
    ) # end of datatables
  })
  
  #Statistics
  output$table2 <- renderPrint({
    tab() %>% 
      summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.)))
  })
  
}


shinyApp(ui, server)
