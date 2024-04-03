library(shiny)

ui <- fluidPage(
  titlePanel("Data Cleaning App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file","Input the file you want to clean.")
    ),
    mainPanel(
      paste("The initial table with NA"),
      tableOutput("table_output"),
      paste("The cleaned table with corresponding emails."),
      tableOutput("cleaned_table")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$data_file)
    read.csv(input$data_file$datapath)
  })
  
  output$table_output <- renderTable({
    data()
  })
  output$cleaned_table <- renderTable({
    dat <- data()
    for(i in 1:ncol(dat)) {
      dat[is.na(dat[,i]), i] <- round(mean(dat[,i],na.rm=TRUE),0)
    }
    
    ## creating an email
    if("first_name" %in% colnames(dat) & "second_name" %in% colnames(dat)) {
      dat$Email <- paste0(dat$first_name,".",dat$second_name,"@gmail.com")
    }
    
    dat
  })
}

shinyApp(ui, server)


