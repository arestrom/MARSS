ui = dashboardPage(
  header = dash_header,
  sidebar = dash_leftsidebar,
  body = dashboardBody(
    includeCSS("www/marss.css"),
    shinyjs::useShinyjs(),
    shinytoastr::useToastr(),
    tabItems(
      tabItem(tabName = "when_where",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Select dates and creel locations",
                  loadingState(),
                  width = 12,
                  height = "800px",
                  closable = FALSE,
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  sidebar = boxSidebar(
                    id = "year_map_sidebar",
                    width = 30,
                    startOpen = TRUE,
                    icon = shiny::icon("bars"),
                    when_where_ui
                  ),
                  leafletOutput("site_map", height = "800px")
                )
              )
      ),
      tabItem(tabName = "data_edits",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Header data",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  survey_ui
                ),
                box(
                  title = "Interview data",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  survey_event_ui
                ),
                box(
                  title = "Fish counts",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  fish_encounter_ui
                ),
                box(
                  title = "Individual fish",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  individual_fish_ui
                )
              )
      ),
      tabItem(tabName = "sample_site",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Edit sample site info",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  sample_site_ui
                )
              )
      ),
      tabItem(tabName = "sampler_info",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Sampler info",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  sampler_ui
                )
              )
      ),
      tabItem(tabName = "survey_query",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Locate surveys",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  survey_query_ui
                )
              )
      ),
      tabItem(tabName = "connect",
              fluidRow(
                br(),
                br(),
                box(
                  title = "Verify and store login values",
                  width = 12,
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  connect_ui
                )
              )
      ),
      tabItem(tabName = "about",
              fluidRow(
                column(width = 2,
                       br(),
                       br(),
                       br(),
                       img(src = "fish_address.png", width = "85%"),
                       br(),
                       br(),
                       includeMarkdown("www/credits.Rmd")
                ),
                column(offset = 1,
                       width = 7,
                       br(),
                       br(),
                       br(),
                       includeMarkdown("www/about.Rmd")
                )
              )
      )
    )
  ),
  title ="Marine Sport Sampling"
)
