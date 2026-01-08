library(shiny)
library(DT)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Gestão de Pacientes e Cálculo de IMC"),
  
  column(12, 
         p("Preencha os dados abaixo. A exportação para Excel e PDF está disponível diretamente nos botões da tabela.")
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Cadastro"),
        textInput("nome", "Nome:"),
        numericInput("altura", "Altura (m):", 1.70, 0.5, step = 0.01),
        numericInput("peso", "Peso (kg):", 70, 1, step = 0.1),
        actionButton("add", "Adicionar", class = "btn-primary", width = "100%")
      )
    ),
    
    mainPanel(
      h4("Lista de Pacientes"),
      # Note que não precisamos mais do downloadButton aqui
      DTOutput("tabela_pacientes")
    )
  )
)

server <- function(input, output, session) {
  
  dados_pacientes <- reactiveVal(data.frame(
    Nome = character(),
    Altura = numeric(),
    Peso = numeric(),
    IMC = numeric(),
    Status = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$add, {
    if (input$nome == "" || is.na(input$altura) || is.na(input$peso)) return()
    
    imc_calc <- round(input$peso / (input$altura^2), 2)
    status_imc <- ifelse(imc_calc < 18.5, "Abaixo do peso",
                         ifelse(imc_calc < 25, "Peso normal",
                                ifelse(imc_calc < 30, "Sobrepeso", "Obesidade")))
    
    nova_linha <- data.frame(Nome = input$nome, Altura = input$altura, 
                             Peso = input$peso, IMC = imc_calc, Status = status_imc)
    
    dados_pacientes(rbind(dados_pacientes(), nova_linha))
    updateTextInput(session, "nome", value = "")
  })
  
  output$tabela_pacientes <- renderDT({
    datatable(
      dados_pacientes(),
      extensions = 'Buttons', # Ativa a extensão de botões
      options = list(
        dom = 'Bfrtip',       # Define a posição dos botões (Buttons, filter, table, info, pagination)
        buttons = list(
          list(extend = 'excel', text = 'Exportar Excel', className = 'btn-success'),
          list(extend = 'pdf', text = 'Exportar PDF'),
          list(extend = 'copy', text = 'Copiar Dados')
        ),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
      )
    )
  })
}

shinyApp(ui, server)