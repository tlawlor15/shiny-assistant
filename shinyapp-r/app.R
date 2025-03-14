library(shiny)
library(bslib)
library(shinychat)
library(ellmer)

app_prompt_template <- readLines("./prompts/app_prompt.md")
app_prompt_r <- readLines("./prompts/app_prompt_r.md")

ui <- page_sidebar(
  header = tags$header(
    shiny::includeCSS("./style.css"),
    tags$title("Shiny Assistant")
    # tags$script(read_file("scripts.js"))
  ),
  sidebar = sidebar(
    div(class = "sidebar-resizer"),
    div(
      style = "width: unset; display: inline-block; padding: 0 20px;",
      selectInput("verbosity", "Verbosity", choices =c("Code only", "Concise", "Verbose"), selected = "Concise")
    ),
    chat_ui("chat", height = "100%"),
    open = "open",
    width = "400px",
    style = "height: 100%;",
    gap = "3px",
    padding = "3px"
  ),
  uiOutput("shinylive_iframe")
  
)

server <- function(input, output, session) {
  
  shiny_panel_visible <- reactiveVal(FALSE)
  shiny_panel_visible_smooth_transition <- reactiveVal(FALSE)
  
  
  llm <- chat_claude(
    model = "claude-3-7-sonnet-latest",
    base_url = ANTHROPIC_API_BASE,
    api_key = ANTHROPIC_API_KEY
  )
  
  app_prompt <- reactive({
    
    verbosity_instructions <- switch(
      input$verbosity,
      "Code only" = "If you are providing a Shiny app, please provide only the code. 
      Do not add any other text, explanations, or instructions unless absolutely
      necessary. Do not tell the user how to install Shiny or run the app, 
      because they already know that.",
      "Concise" = "Be concise when explaining the code. Do not tell the user how to install Shiny or run the app, 
      because they already know that.",
      "Verbose" = ""
    )
    
    prompt <- list(
      language = "R",
      language_specific_prompt = app_prompt_r,
      verbosity = verbosity_instructions()
    )
    
  })
  
}

shinyApp(ui, server)