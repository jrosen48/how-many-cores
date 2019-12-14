f <- function(n_iterations_desired, n_cores, warm_up) {
    iter_per_core <- n_iterations_desired/n_cores
    iter_per_core + warm_up
}

library(dplyr)
library(ggplot2)
library(purrr)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("How Many Cores?"),
    p("This app is designed to be used in the context of Markov Chain Monte Carlo (MCMC) estimation procedures, for which it can be difficult to determine the number of chains and cores to use, even when using distributed computing platforms, because each chain typically requires a warm-up period."),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p("In this section, select the total number of samples that you want from your MCMC output, as well as how many warm-up iterations you want for each chain."),
            numericInput("n_iterations_desired",
                         "No. of Samples Desired",
                         min = 1,
                         max = 10000, 
                         value = 2000),
            # numericInput("n_cores",
            #              "Maximum No. of Cores",
            #              min = 1,
            #              max = 250,
            #              value = 30),
            numericInput("warm_up",
                         "No. of Warm-up Iterations Per Chain",
                         min = 1,
                         max = 5000,
                         value = 500),
            checkboxInput("lots_of_cores",
                          "Lots of cores! (create a plot with from 1 to 1,000 cores, instead of 1 to 100)"),
            actionButton("button", "Run")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            p("In this section, select one of the options for the number of cores to calculate how many iterations per chains (cores) are needed."),
            plotOutput("plot"),
            numericInput("select_n_cores",
                         "Selected No. of Cores (enter this to determine how many iterations per chain are needed)",
                         min = 1,
                         max = 1000, 
                         value = NULL),
            actionButton("button1", "Run"),
            verbatimTextOutput("print"),
            p(),
            tags$a(href="https://github.com/jrosen48/how-many-cores", "Source (GitHub)")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observeEvent(input$button, {
        
        #n_cores = input$n_cores
        warm_up = input$warm_up
        n_iterations_desired = input$n_iterations_desired
        
        if(input$lots_of_cores) {
            n_cores = 1:1000
        } else {
            n_cores = 1:100
        }
        
        d <- tibble(n_cores = n_cores,
                    iterations_per_core = map_dbl(.x = n_cores, f, n_iterations_desired = n_iterations_desired, warm_up = warm_up))
        
        output$plot <- renderPlot( {
            to_plot <- d %>% 
                mutate(total_processing_needed = n_cores * iterations_per_core) %>% 
                mutate(n_cores = ifelse(n_cores < 5, n_cores,
                                        ifelse(n_cores < 40 & (n_cores%%5 == 0 | n_cores == 1), n_cores,
                                               ifelse(n_cores < 100 & n_cores%%10 == 0,n_cores,
                                                      ifelse(n_cores < 1000 & n_cores%%100 == 0, n_cores, NA)))))
            
            to_plot %>% 
                ggplot(aes(y = iterations_per_core, x = total_processing_needed, label = n_cores)) +
                geom_point() +
                ggrepel::geom_text_repel() +
                labs(title = "Interations per core by total iterations needed",
                     subtitle = "The points' labels represent the number of cores") +
                xlab("Total Iterations Needed (Cost)") +
                ylab("Iterations Per Core (Time)") +
                theme_bw()
            
        })
        
    })
    
    observeEvent(input$button1, {

        warm_up = input$warm_up
        n_iterations_desired = input$n_iterations_desired
        #n_cores <- input$n_cores
        select_n_cores = input$select_n_cores
        # 
        # d <- tibble(n_cores = n_cores,
        #             iterations_per_core = map_dbl(.x = n_cores, f, n_iterations_desired = n_iterations_desired, warm_up = warm_up))

        output$print <- renderPrint( {
            o <- f(n_iterations_desired, select_n_cores, warm_up)
            paste0("Number of iterations per chain: ", ceiling((o / select_n_cores) + warm_up))
        })

    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
