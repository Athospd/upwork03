#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny shinymaterial
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    material_page(
      title = "Basic Page + Side-Nav + Tabs",
      # Place side-nav in the beginning of the UI
      material_side_nav(
        fixed = FALSE,
        tags$h3("Side-Nav Content")
      ),
      # Define tabs
      material_tabs(
        tabs = c(
          "First Tab" = "first_tab",
          "Second Tab" = "second_tab"
        )
      ),
      # Define tab content
      material_tab_content(
        tab_id = "first_tab",
        tags$h1("First Tab Content")
      ),
      material_tab_content(
        tab_id = "second_tab",
        tags$h1("Second Tab Content")
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'upwork03'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

