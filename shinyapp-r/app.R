library(shiny)
library(bslib)
library(shinychat)
library(ellmer)
library(stringr)
library(shinyjs)

readRenviron("./.Renviron")

app_prompt_template_raw <- readLines("./prompts/app_prompt.md")
app_prompt_r <- readLines("./prompts/app_prompt_r.md")

app_prompt_template <- paste(app_prompt_template_raw, collapse = "\n")

ANTHROPIC_API_BASE <- Sys.getenv("ANTHROPIC_API_BASE") |> 
  paste0("/v1")
  
# ANTHROPIC_API_KEY <- Sys.getenv("ANTHROPIC_API_KEY")

# SHINYLIVE_BASE_URL <- "https://shinylive.io/"

greeting <- "Hello, I'm Shiny Assistant! I'm here to help you with [Shiny](https://shiny.posit.co), a web framework for data driven apps. You can ask me questions about how to use Shiny,
to explain how certain things work in Shiny, or even ask me to build a Shiny app for
you.

Here are some examples:

- 'How do I add a plot to an application?'
- 'Create an app that shows a normal distribution.'
- 'Show me how make it so a table will update only after a button is clicked.'
- Ask me, 'Open the editor', then copy and paste your existing Shiny code into the editor, and then ask me to make changes to it.

Let's get started! 🚀

<div class='position-relative'>
  <div class='position-absolute start-50 translate-middle rounded-pill badge border border-default text-bg-light text-center'
      style='font-weight: normal; cursor: pointer;'
      data-bs-toggle='popover'
      title='Privacy notice'
      data-bs-content='The Shiny team reserves the right to log your conversations and use them to fine-tune Shiny Assistant and improve Shiny. Also, all conversation activity and Shinylive editor content in this window will be sent to APIs controlled by Anthropic PBC. Please do not use Shiny Assistant for sensitive work!'>
    Who can see my activity?
    <svg xmlns='http://www.w3.org/2000/svg' width='14' height='14' fill='currentColor' class='bi bi-info-circle' viewBox='0 0 16 16'>
      <path d='M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14m0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16'/>
      <path d='m8.93 6.588-2.29.287-.082.38.45.083c.294.07.352.176.288.469l-.738 3.468c-.194.897.105 1.319.808 1.319.545 0 1.178-.252 1.465-.598l.088-.416c-.2.176-.492.246-.686.246-.275 0-.375-.193-.304-.533zM9 4.5a1 1 0 1 1-2 0 1 1 0 0 1 2 0'/>
    </svg>
  </div>
</div>"

ui <- page_sidebar(
  header = tags$header(
    shiny::includeCSS("./style.css"),
    tags$head(
      tags$title("Shiny Assistant"),
      tags$script("./scripts.js")
      )
  ),
  sidebar = sidebar(
    div(class = "sidebar-resizer"),
    div(
      style = "width: unset; display: inline-block; padding: 0 20px;",
      selectInput("verbosity", "Verbosity", choices = c("Code only", "Concise", "Verbose"), selected = "Concise")
    ),
    # chat_ui(
    #   "chat", 
    #   messages = list(
    #     content = greeting,
    #     role = "assistant"
    #   ),
    #   height = "100%"
    # ),
    open = "open",
    width = "400px",
    style = "height: 100%;",
    gap = "3px",
    padding = "3px"
  ),
  chat_ui(
    "chat", 
    messages = list(
      content = htmltools::HTML(greeting),
      role = "assistant"
    ),
    height = "100%"
  ),
  uiOutput("shinylive_iframe"),
  fillable = TRUE
  
)

server <- function(input, output, session) {
  
  shiny_panel_visible <- reactiveVal(FALSE)
  shiny_panel_visible_smooth_transition <- reactiveVal(TRUE)
  
  # Find the sidebar layout and add a custom CSS class to it
  observe({
    # Use JavaScript to add the class after the UI is rendered
    session$onFlushed(function() {
      runjs("
      const sidebarLayout = document.querySelector('.bslib-page-sidebar .bslib-sidebar-layout');
      if (sidebarLayout) {
        sidebarLayout.classList.add('chat-full-width');
      }
    ")
    })
  })
  
  api_key_modal <- modalDialog(
    title = "Please enter your API key",
    passwordInput('input_api_key', label = NULL, width = "100%", placeholder = "Anthropic API key"),
    easyClose = FALSE,
    fade = FALSE,  
    footer = NULL,
    tags$script(HTML("
    $(document).on('keyup', '#input_api_key', function(e) {
      // Check if the key is Enter (keyCode 13)
      if (e.keyCode === 13 && $(this).val().length > 0) {
        Shiny.setInputValue('api_key_entered', $(this).val());
      }
    });
  "))
  )
  
  showModal(api_key_modal)
  
  observeEvent(input$api_key_entered, {
    removeModal()
  })
  
  verbosity_instructions <- reactive({
    
    if(input$verbosity == "Code only") {
      "If you are providing a Shiny app, please provide only the code. 
           Do not add any other text, explanations, or instructions unless absolutely
           necessary. Do not tell the user how to install Shiny or run the app, 
           because they already know that."
    } else if(input$verbosity == "Concise") {
      "Be concise when explaining the code. Do not tell the user how to install Shiny or run the app, 
           because they already know that."
    } else {
      ""
    }
  })
  
  app_prompt <- reactive({
    
    test <- str_glue(
      app_prompt_template,
      language = "R",
      language_specific_prompt = app_prompt_r,
      verbosity = verbosity_instructions()
    )
  })
  
  
  llm <- reactive({
    req(input$input_api_key)
    
    chat_claude(
      model = "claude-3-7-sonnet-latest",
      system_prompt = app_prompt(),
      base_url = ANTHROPIC_API_BASE,
      api_key = isolate(input$input_api_key)
    )
  })
  
  observeEvent(input$chat_user_input, {
    stream <- llm()$stream(input$chat_user_input)
    chat_append("chat", stream)
  })
  
}

shinyApp(ui, server)