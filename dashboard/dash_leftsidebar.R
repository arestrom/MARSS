#==========================================================================
# ShinyDashboardPlus left sidebar
#==========================================================================

dash_leftsidebar = dashboardSidebar(
  width = 175,
  collapsed = FALSE,
  div(textOutput("Welcome"), style = "padding: 20px"),
  sidebarMenu(
    id = "tabs",
    menuItem("When and where", tabName = "when_where", icon = icon("globe")),
    menuItem("Add or edit data", tabName = "data_edits", icon = icon("database")),
    menuItem("Edit creel site", tabName = "sample_site", icon = icon("map-pin")),
    menuItem("Sampler info", tabName = "sampler_info", icon = icon("user")),
    menuItem("Survey query", tabName = "survey_query", icon = icon("share-square")),
    menuItem("Connect", tabName = "connect", icon = icon("user-lock")),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)
