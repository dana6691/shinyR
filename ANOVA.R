if (!require("pacman")) install.packages("pacman")
pacman::p_load("party", "car", "shinythemes","DT","plotly","RColorBrewer","RColorBrewer","lubridate","leaflet","ggplot2","shiny")
library(shiny)
library(ggplot2)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(DT)
library(shinythemes)
library(car)
library(party)

#https://www.statmethods.net/stats/anova.html #MANOVA
# http://rpubs.com/acolumbus/oscon18_rshinytutorial/ 
# https://gupsych.github.io/tquant/data-input.html#resources
# https://gallery.shinyapps.io/CDCPlot/
# http://rstudio.github.io/shiny/tutorial/#reactivity-overview
# https://datacarpentry.org/dc_zurich/R-ecology/05-visualisation-ggplot2.html ggplot
# https://medium.com/@CharlesBordet/how-to-deploy-a-shiny-app-on-aws-part-1-4893d0a7432f AWS

library(multcomp) #tukey test 
runApp(
  list(
    ui = fluidPage(
      headerPanel('Analysis of Variance'),
      sidebarPanel(
        fileInput("file1", "CSV file", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        checkboxInput("header", "Header", TRUE),
        radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),','),
        selectInput('type', 'Sums of Squares type', 
                    c(I = 'type1', II = 'type2', III = 'type3'), 'type1'),
        numericInput("CL", "Confidence Level",0.1, min=0, max=1)
        ,uiOutput('var'),
        a("Click here!", href = "http://www.listendata.com/") #creating a hyperlink
      )
      , mainPanel( 
        tabsetPanel(type = "tabs", id = "tabs",
                    tabPanel("Data",DT::dataTableOutput(outputId = "Datatable")),
                    tabPanel("Assumption Test", h4("Equal Variance Test"), tableOutput(outputId = "variance"),h4("Normality Test"), tableOutput(outputId = "normal"),plotOutput(outputId = "normality")),
                    tabPanel('ANOVA Table',tableOutput('aovSummary')),
                    tabPanel("Box Plot", plotOutput(outputId = "boxplot")),
                    tabPanel("Multiple Comparison",plotOutput(outputId = "multp"),verbatimTextOutput("pair"),plotOutput(outputId = "ci"))
        )
      )
    )
    , 
    server = function(input, output, session) {
      #file
      csvfile <- reactive({
        csv <- input$file1
        if (is.null(csv)){return(NULL)}
        dt <- read.csv(csv$datapath, header=input$header, sep=input$sep)
        dt
      })
      #variable
      output$var <- renderUI({
        if(is.null(input$file1$datapath)){
          return()
        }else{
          
          list (selectInput("dvar", "Dependent Variable", choices =   names(csvfile()[, sapply(csvfile(), class) == "numeric"])),
                selectInput("ivar", "Independent Variable", choices = csvfile()%>% dplyr::select_if(is.factor) %>% names()),
                selectInput("cvar", "Color by", choices = names(csvfile())),
                actionButton("submit", "Submit")
          )
        }
      })


      #ANOVA summary
      output$aovSummary = renderTable({
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          if(input$type == 'type1'){
            return(isolate(anova(lm(csvfile()[,input$dvar] ~ csvfile()[,input$ivar], data = csvfile()))))
          }
          if(input$type == 'type2'){
            return(isolate(Anova(lm(csvfile()[,input$dvar] ~ csvfile()[,input$ivar], data = csvfile())), Type = "II", test.statistic = "F"))
          }
          if(input$type == 'type3'){
            isolate(
              fit <- aov(csvfile()[,input$dvar] ~ csvfile()[,input$ivar], data = csvfile())
            )
            return(drop1(fit, ~ . , test = 'F'))
          }
        }
      })
      #multiple plot
      output$multp <- renderPlot({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          ind <- csvfile()[,input$ivar]
          fit1 <- aov(csvfile()[,input$dvar] ~ ind, data = csvfile())
          fit2<- summary(glht(fit1 , linfct = mcp(ind = "Tukey")))
          fit3 <- cld(fit2,level=input$CL)
          return(plot(fit3, xlab="Treatment"))
        }
      })
      #Pairwise
      output$pair <- renderPrint({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          ind <- csvfile()[,input$ivar]
          fit1 <- aov(csvfile()[,input$dvar] ~ ind, data = csvfile())
          fit2<- summary(glht(fit1 , linfct = mcp(ind = "Tukey")))
          return(fit2)
        }
      })
      #confidence interval plot
      output$ci <- renderPlot({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          ind <- csvfile()[,input$ivar]
          fit1 <- aov(csvfile()[,input$dvar] ~ ind, data = csvfile())
          fit2<- confint(glht(fit1 , linfct = mcp(ind = "Tukey")))
          fit3 <- print(fit2)
          return(plot(fit3, xlab="Treatment"))
        }
      })
      # Create the boxplot object the plotOutput function is expecting
      output$boxplot <- renderPlot({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
        return(ggplot(data = csvfile(), aes_string(x = csvfile()[,input$ivar], y = csvfile()[,input$dvar], fill=csvfile()[,input$cvar])) + # x, y, and color layers of the plot depend on user input
          scale_fill_brewer(palette="Spectral") +
          geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4))
        }
      })
      # Homogeneity Test
      output$variance <- renderTable({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          return(leveneTest(csvfile()[,input$dvar] ~ csvfile()[,input$ivar], data = csvfile()))
        }
      })
      # Normality Test
      output$normal <- renderTable({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          norm <- unlist(shapiro.test(csvfile()[,input$dvar]))
          return(print(norm))
        }
      })
      # Normality Test
      output$normality <- renderPlot({ 
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          fit1 <- aov(csvfile()[,input$dvar] ~ csvfile()[,input$ivar], data = csvfile())
          par(mfrow=c(1,2))
          return(list(plot(fit1,1),plot(fit1,2)))
        }
      })
      # Data Table
      output$Datatable <- DT::renderDataTable({
        if(is.null(input$file1$datapath)){return()}
        if(input$submit > 0){
          return(DT::datatable(data = csvfile(),
                      options = list(pageLength = 20),
                      rownames = F))
        }  
      })
    })
)