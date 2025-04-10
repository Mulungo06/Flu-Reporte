library(shiny)
library(plotly)
library(here)
library(openxlsx)
library(lubridate)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Dados Demográficos Influenza e RSV"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Base de Dados Disa", accept = ".xlsx"),
      radioButtons("dataType", "Selecione o Tipo de Dados (FLU ou RSV):",
                   choices = c("Influenza" = 0, "RSV" = 1)),
      fileInput("file2", "Base de Dados", accept = ".xlsx"),
      downloadButton("downloadData", "Dados Demográficos"),
      
      conditionalPanel(
        condition = "input.dataType == 0",
        checkboxGroupInput('virusType', label = h4('Tipos de Virus:'), choices = NULL), 
        checkboxGroupInput("virusSubtype", label = h4("Subtipos de Influenza"), choices = NULL)
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tableOutput("contents"),
      conditionalPanel(
        condition = "input.dataType == 0",
        tabsetPanel(
          tabPanel(h5("Circulação de IRAs"),
                   plotlyOutput("linePlot1")
          ),
          tabPanel(h5("Subtipos Circulantes"),
                   plotlyOutput("linePlot2")
          ),
          tabPanel(h5("Distribuição de IRAs por Grupos Etários"),
                   plotlyOutput("combinedPlots")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  source(here("Influenza_report_function.R"))
  source(here("RSV_report_function.R"))
  source(here("Influenza_clean_long_data.R"))
  
  # Reactive function to clean and combine data
  combined_data <- reactive({
    req(input$file1, input$file2)
    if (input$dataType == 0) {
      clean_flu_data(input$file1$datapath, input$file2$datapath)
    } else {
      clean_rsv_data(input$file1$datapath, input$file2$datapath)
    }
  })
  
  # Render the table output
  output$contents <- renderTable({
    req(combined_data())
    tail(combined_data(), 5)
  })
  
  # Download handler for exporting data
  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$dataType == 0) {
        paste("dados_demograficos_Flu_", Sys.Date(), ".xlsx", sep = "")
      } else {
        paste("dados_demograficos_RSV_", Sys.Date(), ".xlsx", sep = "")
      }
    },
    content = function(file) {
      openxlsx::write.xlsx(combined_data(), file)
    }
  )
  
  # Reactive function to process Influenza data if selected
  df_long_flu_p <- reactive({
    req(input$dataType, combined_data())
    if (input$dataType == 0) {
      clean_flu_long_data(combined_data())
    } else {
      NULL  # Return NULL if RSV data selected
    }
  })
  
  observe({
    if (input$dataType == 0) {
      updateCheckboxGroupInput(session, 'virusType', choices = unique(isolate(df_long_flu_p()$virus_type)), selected = c("Influenza A", 'Influenza B')) 
      updateCheckboxGroupInput(session, 'virusSubtype', choices = unique(isolate(df_long_flu_p()$influenza_subtype)), selected = c("H3N2", "H1N1 Pand2009", "Victoria"))
    } else {
      updateCheckboxGroupInput(session, 'virusType', choices = NULL)
      updateCheckboxGroupInput(session, 'virusSubtype', choices = NULL)
    }
  })
  
  # Render Influenza circulation plot
  output$linePlot1 <- renderPlotly({
    req(df_long_flu_p())
    
    counts_influenza_type <- df_long_flu_p() %>%
      filter(resultado_virus_long == 'Positivo', virus_type %in% input$virusType) %>%
      group_by(Week, virus_type) %>%
      summarise(count = n())
    
    plot <- ggplot(counts_influenza_type, aes(x = Week, y = count, colour = virus_type)) +
      geom_line() + labs(title = 'Circulação de IRAs em Moçambique',
                         x = 'Semana Epidemiológica', y = 'Positividade', colour = "Tipo de Vírus") +
      theme_gray()
    
    ggplotly(plot)
  })
  
  # Render Influenza subtype circulation plot
  output$linePlot2 <- renderPlotly({
    req(df_long_flu_p())
    
    counts_influenza_subtype <- df_long_flu_p() %>%
      filter(resultado_virus_long == 'Positivo', virus_type %in% input$virusType) %>%
      filter(resultado_flu_subtype == 'Positivo', influenza_subtype %in% input$virusSubtype) %>%
      group_by(Week, virus_type, influenza_subtype) %>%
      summarise(count = n())
    
    plot <- ggplot(counts_influenza_subtype, aes(x = Week, y = count, colour = influenza_subtype)) +
      geom_line() + labs(title = 'Circulação de Subtipos do Vírus Influenza em Moçambique', 
                         x = 'Semana Epidemiológica', y = 'Positividade', colour = 'Subtipo de Influenza') +
      theme_gray()
    
    ggplotly(plot)
  })
  
  # Render age group distribution plot
  output$combinedPlots <- renderPlotly({
    req(df_long_flu_p())
    
    counts_age_groups_influenza_type <- df_long_flu_p() %>%
      filter(resultado_virus_long == 'Positivo', virus_type %in% input$virusType) %>%
      #filter(resultado_flu_subtype == 'Positivo', influenza_subtype %in% input$virusSubtype) %>%
      mutate(faixa_etaria = cut(idade, breaks = c(0, 2, 5, 15, 50, 65, Inf), labels = c('0-<2', '2-<5', '5-<15', '15-<50', '50-<65', '>=65'))) %>%
      group_by(faixa_etaria, virus_type, influenza_subtype) %>%
      summarise(count = n())
    
    plot <- ggplot(counts_age_groups_influenza_type, aes(x = faixa_etaria, y = count, fill = virus_type)) +
      geom_col() + labs(title = 'Circulação de IRAs nos diferentes Grupos Etários',
                        x = 'Grupo Etário', y = 'Positividade', fill = 'IRAs') +
      scale_fill_hue(direction = 1) +
      theme_minimal() +
      facet_wrap(vars(virus_type))
    
    ggplotly(plot)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
