# Ładowanie niezbędnych pakietów
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(ggExtra)
library(knitr)
library(kableExtra)
library(plotly)

# Wczytaj dane z pliku CSV
data <- read_csv("~/Desktop/odkamienianie/naR.csv")

# Upewnij się, że kolumny są odpowiedniego typu
data$wartosc_kamieni <- as.integer(gsub("[^0-9.-]", "", data$wartosc_kamieni))
data <- data %>%
  mutate(
    gmina = as.character(gmina),
    lokalizacja = as.character(lokalizacja),
    pow_kamieni = as.numeric(pow_kamieni),
    wartosc_kamieni = as.numeric(wartosc_kamieni),
    rok = as.numeric(Rok)
  )

# UI (interfejs użytkownika)
ui <- fluidPage(
  titlePanel("Analiza dotacji na odkamienianie gruntów według gmin i obrębów"),
  h3("Dane dotyczące województwa podlaskiego."),
  sidebarLayout(
    sidebarPanel(
      varSelectInput("xvar", "X variable", data %>% select(gmina, lokalizacja), selected = "gmina"),
      varSelectInput("yvar", "Y variable", data %>% select(pow_kamieni, wartosc_kamieni), selected = "pow_kamieni"),
      checkboxGroupInput(
        "year", "Filter by year",
        choices = unique(data$Rok), 
        selected = unique(data$Rok)
      ),
      selectInput("plot_type", "Plot Type", choices = c("Scatter" = "scatter", "Bar" = "bar"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres", plotlyOutput("scatter")),
        tabPanel("Tabela", htmlOutput("table")),
        tabPanel("Wszystkie dane", htmlOutput("data")),
        tabPanel("Statystyki", plotlyOutput("stat")),
        tabPanel("Inny wykres", plotlyOutput("other"))
      )
    )
  )
)

# Server (logika serwera)
server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$year)
    data %>% filter(rok %in% input$year)
  })
  
  aggregated_data <- reactive({
    subsetted() %>%
      group_by_at(vars(input$xvar)) %>%
      summarise(
        mean_y = mean(!!sym(input$yvar), na.rm = TRUE),
        max_y = max(!!sym(input$yvar), na.rm = TRUE),
        min_y = min(!!sym(input$yvar), na.rm = TRUE),
        median_y = median(!!sym(input$yvar), na.rm = TRUE),
        Q1 = quantile(!!sym(input$yvar), 0.25, na.rm = TRUE),
        Q3 = quantile(!!sym(input$yvar), 0.75, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  output$scatter <- renderPlotly({
    p <- ggplot(aggregated_data(), aes_string(x = input$xvar, y = "mean_y", text = input$xvar)) +
      labs(title = paste("Plot of", input$xvar, "vs", input$yvar)) +
      theme_minimal() +
      theme(axis.text.x = element_blank())  # Usuwamy etykiety na osi X
    
    if (input$plot_type == "scatter") {
      p <- p + geom_point() + geom_smooth(method = "lm", se = FALSE)
    } else if (input$plot_type == "bar") {
      p <- p + geom_bar(stat = "identity")
    }
    
    # Dodanie interaktywnych etykiet
    p <- ggplotly(p) %>%
      layout(hovermode = "closest") %>%
      style(hoverinfo = "text", text = ~paste(input$xvar, ": ", !!sym(input$xvar), "<br>Mean ", input$yvar, ": ", mean_y))
    
    p
  })
  
  output$table <- renderUI({
    aggregated_data() %>%
      select(input$xvar, mean_y, max_y, min_y, median_y, Q1, Q3) %>%
      rename(
        `Mean` = mean_y,
        `Max` = max_y,
        `Min` = min_y,
        `Median` = median_y,
        `1st Quartile` = Q1,
        `3rd Quartile` = Q3
      ) %>%
      kable() %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = F,
        position = "left",
        font_size = 12,
        fixed_thead = TRUE,
        row_label_position = "inline"
      ) %>%
      column_spec(1, width = "30%") %>%
      column_spec(2:7, width = "15%") %>%
      as.character() %>%
      HTML()
  })
  
  output$data <- renderUI({
    subsetted() %>%
      mutate(Lp = row_number()) %>% # Dodanie kolumny liczby porządkowej
      select(Lp, everything()) %>%
      select(Lp, gmina, lokalizacja, pow_kamieni, wartosc_kamieni, rok) %>%
      kable() %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = F,
        position = "left",
        font_size = 12,
        fixed_thead = TRUE,
        row_label_position = "inline"
      ) %>%
      as.character() %>%
      HTML()
  })
  
  output$stat <- renderPlotly({
    suma_wartosci <- data %>%
      group_by(gmina) %>%
      summarise(suma_wartosc_kamieni = sum(wartosc_kamieni, na.rm = TRUE)) %>%
      arrange(desc(suma_wartosc_kamieni))
    
    p <- ggplot(suma_wartosci, aes(x = reorder(gmina, -suma_wartosc_kamieni), y = suma_wartosc_kamieni)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Wartość kamieni według gmin",
           x = "Gmina",
           y = "Suma wartości kamieni") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      geom_text(aes(label = suma_wartosc_kamieni), vjust = 0, angle = 90)
    
    ggplotly(p)
  })
  
  output$other <- renderPlotly({
    filtered_data <- data %>%
      group_by(gmina) %>%
      summarise(suma_wartosc_kamieni = sum(wartosc_kamieni, na.rm = TRUE)) %>%
      arrange(desc(suma_wartosc_kamieni))
    
    # Utwórz boxplot
    p <- ggplot(filtered_data, aes(y = suma_wartosc_kamieni)) +
      geom_boxplot(fill = "skyblue") +
      labs(title = "Wartość kamieni według gmin",
           y = "Suma wartości kamieni",
           x = "") +
      theme_minimal()
    
    # Oblicz podstawowe dane statystyczne
    rok_najwiecej_pow_kamieni <- data %>%
      group_by(rok) %>%
      summarise(suma_pow_kamieni = sum(pow_kamieni, na.rm = TRUE)) %>%
      arrange(desc(suma_pow_kamieni)) %>%
      slice_head(n = 10)
    
    rok_najwieksza_wartosc_kamieni <- data %>%
      group_by(rok) %>%
      summarise(suma_wartosc_kamieni = sum(wartosc_kamieni, na.rm = TRUE)) %>%
      arrange(desc(suma_wartosc_kamieni)) %>%
      slice_head(n = 10)
    
    # Wyświetl dane statystyczne w konsoli
    print(rok_najwieksza_wartosc_kamieni)
    
    ggplotly(p)
  })
  
}

shinyApp(ui, server)
