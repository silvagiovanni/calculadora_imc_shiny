library(shiny)
library(DT)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Gestão de Pacientes e Cálculo de IMC"),
  
  column(12, 
         p("Preencha os dados e clique em Adicionar. Para remover registros, selecione as linhas na tabela e clique em 'Apagar Linha(s)'. ",
           "Você também pode exportar os dados (Excel ou PDF) ou copiá-los para a área de transferência usando os botões acima da tabela.")
  ),
  
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        h4("Cadastro"),
        textInput("nome", "Nome:"),
        numericInput("altura", "Altura (m):", 1.70, 0.5, step = 0.01),
        numericInput("peso", "Peso (kg):", 70, 1, step = 0.1),
        actionButton("add", "Adicionar Paciente", class = "btn-primary", width = "100%", icon = icon("plus"))
      )
    ),
    
    mainPanel(
      div(style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Lista de Pacientes"),
          # Botão de apagar (só aparece se houver seleção)
          uiOutput("botao_deletar_ui")
      ),
      DTOutput("tabela_pacientes")
    )
  )
)

server <- function(input, output, session) {
  
  # Banco de dados reativo
  dados_pacientes <- reactiveVal(data.frame(
    Nome = character(),
    Altura = numeric(),
    Peso = numeric(),
    IMC = numeric(),
    Status = character(),
    stringsAsFactors = FALSE
  ))
  
  # Adicionar paciente
  observeEvent(input$add, {
    if (input$nome == "" || is.na(input$altura) || is.na(input$peso)) {
      showNotification("Preencha todos os campos.", type = "warning")
      return()
    }
    
    imc_calc <- round(input$peso / (input$altura^2), 2)
    status_imc <- ifelse(imc_calc < 18.5, "Abaixo do peso",
                         ifelse(imc_calc < 25, "Peso normal",
                                ifelse(imc_calc < 30, "Sobrepeso", "Obesidade")))
    
    nova_linha <- data.frame(Nome = input$nome, Altura = input$altura, 
                             Peso = input$peso, IMC = imc_calc, Status = status_imc)
    
    dados_pacientes(rbind(dados_pacientes(), nova_linha))
    updateTextInput(session, "nome", value = "")
  })
  
  # Lógica para deletar linhas
  observeEvent(input$delete_rows, {
    ids_selecionados <- input$tabela_pacientes_rows_selected
    if (length(ids_selecionados) > 0) {
      dados_pacientes(dados_pacientes()[-ids_selecionados, ])
    }
  })
  
  # Botão de deletar condicional
  output$botao_deletar_ui <- renderUI({
    if (!is.null(input$tabela_pacientes_rows_selected)) {
      actionButton("delete_rows", "Apagar Linha(s)", class = "btn-danger", icon = icon("trash-alt"))
    }
  })
  
  # Tabela com Botões de Exportação e Copiar
  output$tabela_pacientes <- renderDT({
    datatable(
      dados_pacientes(),
      selection = 'multiple',
      extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
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