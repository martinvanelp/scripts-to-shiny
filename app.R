library(shiny)
library(rmarkdown)
library(ggplot2)

availableScripts <- list.files("./scripts")

# UI ----
ui <- fluidPage(
    
    titlePanel(""),
    
    # Sidebar
    sidebarLayout(
        sidebarPanel(
            includeMarkdown("README.md"),
            selectInput(inputId = "script",
                        label   = "Script",
                        choices = availableScripts,
                        multiple = FALSE)
        ),
        
        # Show a plot and underlying "script"
        mainPanel(
            h2("Plot"),
            plotOutput("plot"),
            
            h2("Script"),
            textOutput("script")
        )
    )
)

# SERVER ----
server <- function(input, output) {
    
    dat <- reactive({
        source(paste0("./scripts/", input$script))
    })
    
    output$plot <- renderPlot({
        
        # inputs
        table <- dat()$value$table
        x <- dat()$value$x
        y <- dat()$value$y
        color <- dat()$value$color
        type <- dat()$value$type
        
        # plot
        if(!is.null(color)) {
            p <- ggplot(table, 
                        aes(x = !!ensym(x), 
                            y = !!ensym(y), 
                            color = !!ensym(color)))
        } else {
            p <- ggplot(table, 
                        aes(x = !!ensym(x), 
                            y = !!ensym(y)))
        }
        
        p <- p + switch(type,
                        line = geom_line(),
                        point = geom_point())
        
        p <- p + theme_minimal()
        
        p
    })
    
    # Script to text
    script <- reactive({
        readLines(paste0("./scripts/", input$script))
    })
    
    output$script <- renderText({ script() })
}

# Run the application 
shinyApp(ui = ui, server = server)

