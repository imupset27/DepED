# app.R
library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)

# ---- MODULES ----
source("modules/mod_deped.R")
source("modules/mod_deped_sf1.R")
source("modules/mod_deped_sf9.R")

# -------------------- UI --------------------
ui <- bs4DashPage(
  title = "EMLStat",
  
  header = dashboardHeader(
    title = tags$div(
      tags$img(src = "emlstat_logo.png", height = "35px", style = "margin-right:10px;"),
      "EMLStat Analytics"
    )
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("DepEd Reports", tabName = "deped", icon = icon("school")),
      menuItem("Statistical Tools", tabName = "stat", icon = icon("chart-line")),
      menuItem("Research Dashboard", tabName = "research", icon = icon("flask")),
      menuItem("Consulting & Training", tabName = "consult", icon = icon("chalkboard-teacher")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  
  body = dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        body, .content-wrapper { background-color: #f8f9fa !important; }
        h1, h2, h3 { font-family: 'Segoe UI', sans-serif; }
        .hero-section {
          background: linear-gradient(120deg, #0d6efd, #0b5ed7);
          color: white;
          padding: 60px 30px;
          text-align: center;
          border-radius: 10px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        .service-card {
          transition: transform 0.2s, box-shadow 0.2s;
          border-radius: 12px;
          cursor: pointer;
        }
        .service-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 6px 20px rgba(0,0,0,0.15);
        }
        .service-icon {
          font-size: 40px;
          color: #0d6efd;
        }
        .footer {
          text-align: center;
          color: #6c757d;
          margin-top: 50px;
          padding: 20px;
          border-top: 1px solid #dee2e6;
        }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "home",
        fluidPage(
          tags$div(
            class = "hero-section",
            tags$h1("Empowering Decisions Through Data"),
            tags$h4("Your trusted partner in statistical consulting, analytics, and research solutions.")
          ),
          
          br(),
          tags$h2(class = "text-center mb-4 text-primary", "Our Services"),
          
          fluidRow(
            column(6,
                   tags$div(
                     class = "service-card",
                     style = "padding: 20px;",
                     onclick = "Shiny.setInputValue('open_deped', true, {priority: 'event'})",
                     bs4Card(
                       title = tagList(icon("school", class = "service-icon"), " DepEd Reports & Analytics"),
                       status = "primary", width = 12,
                       "We build and automate data-driven reports aligned with the Department of Education’s standards to support evidence-based decision making in schools.",
                       br(), tags$div("Explore Service", class = "btn btn-primary mt-2")
                     )
                   )
            ),
            column(6,
                   tags$div(
                     class = "service-card",
                     style = "padding: 20px;",
                     onclick = "Shiny.setInputValue('open_stat', true, {priority: 'event'})",
                     bs4Card(
                       title = tagList(icon("chart-line", class = "service-icon"), " Statistical Tools & Modeling"),
                       status = "info", width = 12,
                       "We provide advanced statistical models, forecasting tools, and automated data analytics applications for research and institutional analysis.",
                       br(), tags$div("Explore Service", class = "btn btn-info mt-2")
                     )
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   tags$div(
                     class = "service-card",
                     style = "padding: 20px;",
                     onclick = "Shiny.setInputValue('open_research', true, {priority: 'event'})",
                     bs4Card(
                       title = tagList(icon("flask", class = "service-icon"), " Research Data Dashboards"),
                       status = "success", width = 12,
                       "Interactive dashboards for exploring research datasets, visualizing trends, and presenting insights to stakeholders effectively.",
                       br(), tags$div("Explore Service", class = "btn btn-success mt-2")
                     )
                   )
            ),
            column(6,
                   tags$div(
                     class = "service-card",
                     style = "padding: 20px;",
                     onclick = "Shiny.setInputValue('open_consult', true, {priority: 'event'})",
                     bs4Card(
                       title = tagList(icon("chalkboard-teacher", class = "service-icon"), " Consulting & Training"),
                       status = "warning", width = 12,
                       "Customized workshops and consulting for schools, universities, and organizations to enhance data literacy and research capacity.",
                       br(), tags$div("Contact Us", class = "btn btn-warning mt-2")
                     )
                   )
            )
          ),
          
          br(),
          tags$h2(class = "text-center text-primary mt-5 mb-3", "Who We Are"),
          tags$p(
            class = "lead text-center mx-auto", style = "max-width:800px;",
            "EMLStat Analytics & Consulting is a statistical solutions company that helps organizations transform raw data into actionable insights.
             We specialize in education analytics, research dashboards, and statistical consulting to empower data-driven decision making."
          ),
          
          tags$div(
            class = "footer",
            HTML("© 2025 <b>EMLStat Analytics & Consulting</b> | 
                 <i class='fa fa-envelope'></i> info@emlstat.com | 
                 <i class='fa fa-phone'></i> +63 912 345 6789")
          )
        )
      ),
      
      tabItem(tabName = "deped", mod_deped_ui("deped")),
      tabItem(tabName = "stat", fluidPage(tags$h3("Statistical Tools (Module Placeholder)"))),
      tabItem(tabName = "research", fluidPage(tags$h3("Research Dashboard (Module Placeholder)"))),
      tabItem(tabName = "consult", fluidPage(tags$h3("Consulting & Training"), tags$p("Contact us at info@emlstat.com for project collaborations and workshops."))),
      tabItem(tabName = "about", fluidPage(tags$h2("About EMLStat"), tags$p("EMLStat Analytics & Consulting helps institutions make better decisions using data.")))
    )
  )
)

# -------------------- SERVER --------------------
server <- function(input, output, session) {
  observeEvent(input$open_deped, updateTabItems(session, "tabs", "deped"))
  observeEvent(input$open_stat, updateTabItems(session, "tabs", "stat"))
  observeEvent(input$open_research, updateTabItems(session, "tabs", "research"))
  observeEvent(input$open_consult, updateTabItems(session, "tabs", "consult"))
  
  mod_deped_server("deped")
}

shinyApp(ui, server)
