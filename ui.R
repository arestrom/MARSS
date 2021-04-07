ui = dashboardPagePlus(
  shinyjs::useShinyjs(),
  shinytoastr::useToastr(),
  enable_preloader = TRUE,
  header = dash_header,
  sidebar = dash_leftsidebar,
  body = dashboardBody(
    includeCSS("www/baseline_data.css"),
    tabItems(
      tabItem(tabName = "crc_site",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Select dates and creel locations",
                  closable = FALSE,
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  collapsed = FALSE,
                  width = 12,
                  enable_sidebar = TRUE,
                  sidebar_width = 25,
                  sidebar_start_open = TRUE,
                  sidebar_content = crc_site_ui,
                  leafletOutput("site_map", height = "800px") %>%
                    shinycssloaders::withSpinner(., size = 0.5)
                )
              )
      ),
      tabItem(tabName = "data_edits",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Header data",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  survey_ui,
                  width = 12
                ),
                boxPlus(
                  title = "Interview data",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  survey_event_ui,
                  width = 12
                ),
                boxPlus(
                  title = "Fish counts",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  fish_encounter_ui,
                  width = 12
                ),
                boxPlus(
                  title = "Individual fish",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  individual_fish_ui,
                  width = 12
                )
              )
      ),
      tabItem(tabName = "sample_site",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Edit sample site info",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  sample_site_ui,
                  width = 12
                )
              )
      ),
      tabItem(tabName = "sampler_info",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Sampler info",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  sampler_ui,
                  width = 12
                )
              )
      ),
      tabItem(tabName = "data_query",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Locate surveys",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  data_query_ui,
                  width = 12
                )
              )
      ),
      tabItem(tabName = "connect",
              fluidRow(
                br(),
                br(),
                boxPlus(
                  title = "Verify and store database credentials",
                  closable = FALSE,
                  collapsible = TRUE,
                  solidHeader = FALSE,
                  collapsed = FALSE,
                  connect_ui,
                  width = 12
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
