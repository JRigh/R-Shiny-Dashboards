#-------------------------------------------
# Shiny app: monthly expenses basic template
#-------------------------------------------

library(shiny)
library(tidyverse)

# Generate artificial monthly expenses dataset
set.seed(123)

housing <- round(runif(12, min = 500, max = 2000), 2)
food <- round(runif(12, min = 100, max = 800), 2)
entertainment <- round(runif(12, min = 50, max = 500), 2)
healthcare <- round(runif(12, min = 50, max = 500), 2)
mobility <- round(runif(12, min = 50, max = 500), 2)
else_expenses <- round(runif(12, min = 50, max = 500), 2)

monthly_expenses <- data.frame(month = paste0("2021-", sprintf("%02d", 1:12)),
                               housing = housing,
                               food = food,
                               entertainment = entertainment,
                               healthcare = healthcare,
                               mobility = mobility,
                               other = else_expenses)

# Define the UI for the shiny app
ui <- fluidPage(
  titlePanel("Monthly Expenses"),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Select Month:", choices = unique(monthly_expenses$month))
    ),
    mainPanel(
      plotOutput("expenses_plot")
    )
  )
)

# Define the server for the shiny app
server <- function(input, output) {
  
  # Create a reactive data frame of monthly expenses for the selected month
  selected_month <- reactive({
    filter(monthly_expenses, month == input$month) %>%
      pivot_longer(cols = -month, names_to = "category", values_to = "value")
  })
  
  # Create a bar plot of expenses for the selected month
  output$expenses_plot <- renderPlot({
    ggplot(selected_month(), aes(x = category, y = value, fill = category)) +
      geom_bar(stat = "identity", width = 0.5) +
      ggtitle(paste0("Monthly Expenses for ", input$month)) +
      xlab("Expense Category") +
      ylab("Expense Amount") +
      theme_bw() +
      theme(legend.position = "none")
  })
  
}

# Run the shiny app
shinyApp(ui = ui, server = server)

#----
# end
#----