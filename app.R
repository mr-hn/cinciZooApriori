library(shiny)
library(shinyjs)
library(tidyverse) # The gasoline for the CaR
library(arules) # Association rules algorithm
library(visNetwork) # Visualizing associations
library(arulesViz)
library(DT) # Print HTML Tables

# DT::datatable parameters
options(DT.options = list(paging = FALSE, # disable pagination
                          scrollY = "300px", # enable vertical scrolling
                          scrollX = TRUE, # enable horizontal scrolling
                          scrollCollapse = TRUE,
                          dom = "t"))  # display just the table

# Data Read ---------------------------------------------------------------
data <- read_csv("files/cinci_zoo_purchases.csv")
# There's a "Food" at the end of each column name. Removing it
colnames(data) <- gsub("Food", "", colnames(data))
# Dropping the transaction ID
data <- data[, -1]
# Find out elements that are not equal to 0 or 1 and change them to 1.
other_vals <- which(!(as.matrix(data) == 1 | as.matrix(data) == 0), arr.ind = TRUE )
data[other_vals] <- 1
data <- as(as.matrix(data), "transactions")

# Build Rules -------------------------------------------------------------
basket_rules <- apriori(data, parameter = list(sup = 0.003, conf = 0.5, 
                                               target = "rules"),
                        control = list(verbose = FALSE)) # Prevent output print


# Data Prep ---------------------------------------------------------------
# Get items in the rules to populate drop down
clean_items <- as(basket_rules, "data.frame") %>% select(rules) %>% 
  separate(rules, c("lhs", "rhs"), sep = "=>") %>% 
  mutate(lhs = str_replace(lhs, "\\{", ""),
         rhs = str_replace(rhs, "\\{", "")) %>% 
  mutate(lhs = str_replace(lhs, "\\}", ""),
         rhs = str_replace(rhs, "\\}", ""))

clean_items_lhs <- clean_items %>% select(lhs) %>% t() %>% as.vector() %>% paste0(collapse = ",") %>%
  toString() %>% strsplit(split = ",")
clean_items_lhs <- clean_items_lhs[[1]] %>% sort() %>% trimws(which = "both") %>% base::unique()


clean_items_rhs <- clean_items %>% select(rhs) %>% t() %>% as.vector() %>% paste0(collapse = ",") %>%
  toString() %>% strsplit(split = ",")
clean_items_rhs <- clean_items_rhs[[1]] %>% sort() %>% trimws(which = "both") %>% base::unique()



# UI ----------------------------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabPanel("Start Here",
           column(4, 
                  wellPanel( 
                    radioButtons("input_type", label = NULL,
                                 choices = c("Understand Purchase Behavior" = "behav",
                                             "Find Recommendations" = "recom"), 
                                 selected = "behav"),
                    selectInput("behav", "Who is buying ____ ?",
                                multiple = FALSE, choices = clean_items_rhs),
                    selectInput("recom", "What else to suggest to ____ buyers?",
                                multiple = FALSE, choices = clean_items_lhs)
                  ))),
  mainPanel(
    p("Clink on a column to sort data by"),
    DT::dataTableOutput("rule_table", width = "100%"),
    br(),
    p("Hover over a 'rule' to see more info"),
    visNetworkOutput("web_plot", width = "100%", height = "300px")
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  observe({
    shinyjs::toggleState("behav", input$input_type == "behav")
    shinyjs::toggleState("recom", input$input_type == "recom")
  })
  
  output$rule_table <- DT::renderDataTable({
    
    if (input$input_type == "behav") {
      req(input$behav)
      arules::subset(basket_rules, subset = rhs %pin% input$behav) %>%
        as("data.frame") %>% separate(rules, c("when visitors ordered", 
                                               "they also ordered"), sep = "=>") %>% 
        mutate(support = round(support, 4),
               confidence = round(confidence, 4),
               lift = round(lift, 4)) %>% 
        # select(2,1,3:6) %>% 
        arrange(desc(confidence)) %>% 
        DT::datatable()
      
    } else {
      req(input$recom)
      arules::subset(basket_rules, subset = lhs %pin% input$recom) %>%
        as("data.frame") %>% separate(rules, c("when visitors ordered", 
                                               "they also ordered"), sep = "=>") %>% 
        mutate(support = round(support, 4),
               confidence = round(confidence, 4),
               lift = round(lift, 4)) %>% 
        arrange(desc(confidence)) %>% 
        DT::datatable()
    }
  })
  
  output$web_plot <- renderVisNetwork({
    
    if (input$input_type == "behav") {
      req(input$behav)
      arules::subset(basket_rules, subset = rhs %pin% input$behav) %>%
        plot(method = "graph",  engine = "htmlwidget") %>% 
        visOptions(nodesIdSelection = FALSE)
        
      
    } else {
      req(input$recom)
      arules::subset(basket_rules, subset = lhs %pin% input$recom) %>% 
        plot(method = "graph",  engine = "htmlwidget") %>% 
        visOptions(nodesIdSelection = FALSE)
    }
  })
}

shinyApp(ui = ui, server = server)