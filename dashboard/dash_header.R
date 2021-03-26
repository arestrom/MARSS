#=============================================================
# ShinyDashboardPlus header function
#=============================================================

dash_header = dashboardHeaderPlus(
  fixed = TRUE,
  title = tagList(
    span(class = "logo-lg", "MarSS"),
    img(src = "ShinyDashboardPlus.svg")),
  enable_rightsidebar = FALSE,
  rightSidebarIcon = "bars"
)


