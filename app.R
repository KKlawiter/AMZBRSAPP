library(tidyverse)
library(readxl)
library(xlsx)
library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(shinythemes)
library(jsonlite)
library(janitor)
library(stringr)
library(ggplot2)
library(purrr)

line_item_columns_needed <- c(
  "line_item_status", "line_item_id", "line_item_budget", "total_cost", "delivery_rate"
)
performance_data_columns_needed <- c(
  "line_item_id", "line_item", "ctr", "total_pixel_cvr",
  "dpvr", "total_units_sold", "total_product_sales", "total_roas"
)

firewake_data_columns_needed <- c(
  "line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", 
  "additional_budget", "new_li_budget", "delivery_rate", "ctr"
)
ctv_data_columns_needed <- c(
  "line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", 
  "additional_budget", "new_li_budget", "delivery_rate", "total_pixel_cvr"
)
consideration_data_columns_needed <- c(
  "line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", 
  "additional_budget", "new_li_budget", "delivery_rate", "dpvr", "total_units_sold", "total_roas"
)
conversion_data_columns_needed <- c(
  "line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", 
  "additional_budget", "new_li_budget", "delivery_rate", "total_units_sold", "total_product_sales", "total_roas"
)

ui <- fluidPage(theme = shinytheme("darkly"),
                tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });'))),
                # App title ----
                titlePanel("Budget Recommendations"),
                sidebarPanel("", width=2,
                             titlePanel(h4("Upload + Merge")),
                             fileInput("line_item_file", "Line Item Data"),
                             fileInput("performance_data_file", "Performance Data"),
                             selectInput("variable", "Choose Order Type",
                                         c("Firewake" = "firewake_data",
                                           "CTV" = "ctv_data",
                                           "Consideration" = "consideration_data",
                                           "Conversion" = "conversion_data"
                                         ), selected="firewake_data"),
                             style = "margin-top:25px", actionButton("merge_data", "Merge Data"),
                             tags$hr(),
                             titlePanel(h4("Budget Info")),
                             numericInput("daysleft",
                                          h4("Days Left"), 
                                          value = 0),
                             numericInput("dailybudget",
                                          h4("Daily Budget"), 
                                          value = 0),
                             numericInput("performancelookback",
                                          h4("Performance Lookback"), 
                                          value = 0),
                             #textOutput("result"),
                             #textOutput("result2"),
                             tags$hr(),
                             style = "margin-top:25px;", downloadButton("download_data", "Download"),
                             tags$head(tags$style("#result{font-size: 15px;}")),
                             tags$head(tags$style("#result2{font-size: 15px;}"))
                ),
                mainPanel(
                  DTOutput("output_table"), style = "font-size:80%", width = 10
                )
)

server <- function(input, output, session) {
  # Calculate the sum of the inputs
  my_sum <- reactive({
    input$daysleft * input$dailybudget
  })
  output$result <- renderText({
    paste(
      # Print the calculated sum
      "Additional Budget", my_sum()
    )
  })
  my_cost <- reactive({
    input$totalcost + (input$daysleft * input$dailybudget)
  })
  output$result2 <- renderText({
    paste(
      # Print the calculated sum
      "New Total Budget", my_cost()
    )
  })
  
  # create placeholder data.frame for merged data
  global <- reactiveValues(merged_data = data.frame())
  
  # create a reactive data that calculates the total row every time the merged data changes
  merged_with_total <- reactive({
    total <- global$merged_data %>%
      summarise(across(where(is.numeric), sum)) %>%
      mutate(line_item_status = "Total") %>%
      select(line_item_status, line_item_budget, total_cost, additional_budget, new_li_budget)
    
    
    if(input$variable == "firewake_data"){
      merged_with_total <- bind_rows(global$merged_data, total) %>%
        select("line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", 
               "additional_budget", "new_li_budget", "delivery_rate", "ctr") 
      colnames(merged_with_total) <-  c("Status", "Line Item ID", "Line Item", "Current Budget", "Total Cost", "Additional %", "New LI Budget", "Delivery Rate", "CTR")
      
    } else if(input$variable == "ctv_data"){
      merged_with_total <- bind_rows(global$merged_data, total) %>%
        select(all_of(ctv_data_columns_needed)) 
      colnames(merged_with_total) <- c("Status", "Line Item ID", "Line Item", "Current Budget", "Total Cost", "Additional %", "New LI Budget", "Delivery Rate","Total Pixel CVR")
      
    } else if(input$variable == "consideration_data"){
      merged_with_total <- bind_rows(global$merged_data, total) %>%
        select(all_of(consideration_data_columns_needed))
      colnames(merged_with_total) <- c("Status", "Line Item ID", "Line Item", "Current Budget", "Total Cost", "Additional %", "New LI Budget", "Delivery Rate", "DPVR", "Total Units Sold",  "ROAS")
      
    } else if(input$variable == "conversion_data"){
      merged_with_total <- bind_rows(global$merged_data, total) %>%
        select(all_of(conversion_data_columns_needed)) 
      colnames(merged_with_total) <- c("Status", "Line Item ID", "Line Item", "Current Budget", "Total Cost", "Additional %", "New LI Budget", "Delivery Rate", "Total Units Sold", "Total Product Sales", "ROAS")
    }
    
    return(merged_with_total)
    
  })
  
  # reading and merging the data.frames when merge_data button is clicked
  observeEvent(
    input$merge_data,
    {
      # Checking if the files are uploaded or not
      if (is.null(input$line_item_file) | is.null(input$performance_data_file)) {
        showNotification(
          "Please upload the Line Item and Performance data excel
          files before you click the 'Merge data' button",
          type = "error"
        )
        #else if (is.null(input$line_item_file) | is.null(input$performance_data_file)) {
          #showNotification(
            #"Please upload the Line Item and Performance data excel
          #files before you click the 'Merge data' button",
           # type = "error"
         # )
      } else {
        # Reading the two excel files
        line_item_data <- read_excel(input$line_item_file$datapath)
        performance_data <- read_excel(input$performance_data_file$datapath)
        
        # Renaming the data with proper cases so we can avoid manual type-case errors in the column names
        line_item_data_proper_names <- janitor::clean_names(line_item_data)
        performance_data_proper_names <- janitor::clean_names(performance_data)
        
        # Merging the data
        global$merged_data <<- line_item_data_proper_names %>%
          select(all_of(line_item_columns_needed)) %>%
          inner_join(performance_data_proper_names %>%
                       select(all_of(performance_data_columns_needed))) %>%
          mutate(
            additional_budget = 5,
            new_li_budget = (((as.numeric(str_replace(additional_budget, pattern = "%", replacement = ""))*my_sum())/100)+(total_cost)),
            new_li_budget = round(new_li_budget, 2),
            ctr = round(ctr * 100, 2),
            total_pixel_cvr = round(total_pixel_cvr * 100, 2),
            dpvr = round(dpvr * 100, 2),
            total_roas = round(total_roas, 2),
            total_cost = round(total_cost, 2)
          ) %>%
          select("line_item_status", "line_item_id", "line_item", "line_item_budget", "total_cost", "additional_budget", "new_li_budget", "delivery_rate", "ctr", "total_pixel_cvr", "dpvr", "total_units_sold", "total_product_sales", "total_roas")
        
        # rendering the datatable
        output$output_table <- renderDT({
          
          # creating color scales
          total_cost_brks <- quantile(global$merged_data$total_cost, probs = seq(.05, .95, .05), na.rm = TRUE)
          total_cost_clrs <- rev(colorRampPalette(c("#99068f","#FFFFFF"))(length(total_cost_brks)+1))
          
          delivery_rate_brks <- quantile(global$merged_data$delivery_rate, probs = seq(.05, .95, .05), na.rm = TRUE)
          delivery_rate_clrs <- rev(colorRampPalette(c("#6010eb","#FFFFFF"))(length(delivery_rate_brks)+1))
          
          ctr_brks <- quantile(global$merged_data$ctr, probs = seq(.05, .95, .05), na.rm = TRUE)
          ctr_clrs <- rev(colorRampPalette(c("#0d86a1","#FFFFFF"))(length(ctr_brks)+1))
          
          total_pixel_cvr_brks <- quantile(global$merged_data$total_pixel_cvr, probs = seq(.05, .95, .05), na.rm = TRUE)
          total_pixel_cvr_clrs <- rev(colorRampPalette(c("#0da17e","#FFFFFF"))(length(total_pixel_cvr_brks)+1))
          
          dpvr_brks <- quantile(global$merged_data$dpvr, probs = seq(.05, .95, .05), na.rm = TRUE)
          dpvr_clrs <- rev(colorRampPalette(c("#b0b00e","#FFFFFF"))(length(dpvr_brks)+1))
          
          total_units_sold_brks <- quantile(global$merged_data$total_units_sold, probs = seq(.05, .95, .05), na.rm = TRUE)
          total_units_sold_clrs <- rev(colorRampPalette(c("#a10f0d","#FFFFFF"))(length(total_units_sold_brks)+1))
          
          total_product_sales_brks <- quantile(global$merged_data$total_product_sales, probs = seq(.05, .95, .05), na.rm = TRUE)
          total_product_sales_clrs <- rev(colorRampPalette(c("#a1590d","#FFFFFF"))(length(total_product_sales_brks)+1))
          
          total_roas_brks <- quantile(global$merged_data$total_roas, probs = seq(.05, .95, .05), na.rm = TRUE)
          total_roas_clrs <- rev(colorRampPalette(c("#11a825","#FFFFFF"))(length(total_roas_brks)+1))
          
          # creating the datatable
          datatable(
            data = merged_with_total(),
            selection = "none",
            editable = list(target = "column", disable = list(columns = c(1,2,3,4,5,7,8,9,10,11,12))),
            extensions = 'Scroller',
            
            options = list(
              scrollX = TRUE,
              scrollY = 600,
              scroller = TRUE
            )
          )%>%
            formatStyle("Total Cost", backgroundColor = styleInterval(total_cost_brks, total_cost_clrs)) %>%
            formatStyle("Delivery Rate", backgroundColor = styleInterval(delivery_rate_brks, delivery_rate_clrs)) %>%
            #formatStyle("ctr", backgroundColor = styleInterval(ctr_brks, ctr_clrs)) %>%
            #formatStyle("dpvr", backgroundColor = styleInterval(dpvr_brks, dpvr_clrs)) %>%
            #formatStyle("total_units_sold", backgroundColor = styleInterval(total_units_sold_brks, total_units_sold_clrs)) %>%
            #formatStyle("total_product_sales", backgroundColor = styleInterval(total_product_sales_brks, total_product_sales_clrs)) %>%
            #formatStyle("total_pixel_cvr", backgroundColor = styleInterval(total_pixel_cvr_brks, total_pixel_cvr_clrs)) %>%
            #formatStyle("total_roas", backgroundColor = styleInterval(total_roas_brks, total_roas_clrs))  %>%
            formatStyle(names(merged_with_total()),"white-space"="nowrap")
        })
      }
    }
  )
  # re calculatig the merged dataset when table cell is edited
  observeEvent(input$output_table_cell_edit, {
    merged_with_total <<- editData(merged_with_total, input$output_table_cell_edit, 'output_table')
  })
  
  
  # download the merged data when download_data button is clicked
  output$download_data <- downloadHandler(
    filename = function() {
      paste("merged_data_", Sys.time(), ".xlsx", sep="")
    },
    content = function(file) {
      data <- as.data.frame(merged_with_total())
      
      write.xlsx(data, file, showNA = FALSE, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
