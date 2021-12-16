#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- \(request) 
  tagList(
    golem_add_external_resources(),
    page_ui()
  )


theme <- \() 
  bslib::bs_add_variables(
    bslib::bs_theme(
      version = "5",
      base_font = bslib::font_google("Roboto"),
      primary = "rgb(229, 229, 229)",
      secondary = "rgb(219, 144, 0)"
    ),
    "btn-border-radius" = "3.5vh",
    "btn-font-size" = "3.5vh"
  )


page_ui <- \() 
  fillPage(
    theme = theme(),
    div(
      class = "page-container", 
      div(
        class = "page",
        phone_ui()
      )
    )
  )


phone_ui <- \() 
  div(
    class = "phone",
    screen_ui(),
    button_pad_ui()
  )


screen_ui <- \() 
  div(
    class = "screen",
    upper_screen_ui(),
    lower_screen_ui()
  )


upper_screen_ui <- \() 
  div(
    class = "upper-screen",
    uiOutput("upper_screen")
  )


lower_screen_ui <- \() 
  div(
    class = "lower-screen",
    uiOutput("lower_screen")
  )


button_pad_ui <- \() 
  div(
    class = "button-pad",
    add(), 
    subtract(), 
    multiply(), 
    divide(),
    seven(),
    eight(),
    nine(),
    clear(),
    four(),
    five(),
    six(),
    plus_minus(),
    one(),
    two(),
    three(),
    percent(),
    zero(),
    decimal(),
    equals()
  )


add <- \() 
  button_symbol_circle("add", "+")


subtract <- \() 
  button_symbol_circle("subtract", "-")


multiply <- \() 
  button_symbol_circle("multiply", "\U00D7")


divide <- \() 
  button_symbol_circle("divide", "\U00F7")


seven <- \() 
  button_number("seven", "7")


eight <- \() 
  button_number("eight", "8")


nine <- \() 
  button_number("nine", "9")


clear <- \() 
  button_symbol_circle("clear", "C")


four <- \() 
  button_number("four", "4")


five <- \() 
  button_number("five", "5")


six <- \() 
  button_number("six", "6")


plus_minus <- \() 
  button_symbol_circle("plus_minus", "\U00B1")


one <- \() 
  button_number("one", "1")


two <- \() 
  button_number("two", "2")


three <- \() 
  button_number("three", "3")


percent <- \() 
  button_symbol_circle("percent", "%")


zero <- \() 
  button_number("zero", "0")


decimal <- \() 
  button_symbol_circle("decimal", ".")


equals <- \() 
  button_symbol_pill("equals", "=")


button_symbol_circle <- \(...) 
  purrr::partial(button_symbol, width = "7vh")(...)


button_symbol_pill <- \(...) 
  purrr::partial(button_symbol, width = "16vh")(...)


button_symbol <- \(...) 
  purrr::partial(button, colored = TRUE)(...)


button_number <- \(...) 
  purrr::partial(
    button,
    width = "7vh",
    colored = FALSE
  )(...)


button <- \(input_id, label, width, colored) {
  font_color <- 
    if (colored) 
      "rgb(255, 255, 255)"
  
    else 
      "rgb(32, 34, 37)"
  
  class <- 
    if (colored) 
      "btn-secondary"
    else 
      "btn-primary"
  
  style <- 
    stringr::str_glue(
      "
      width: {width}; 
      height: 7vh;
      color: {font_color};
      "
    )
  
  div(
    class = input_id,
    actionButton(
      inputId = input_id,
      label = label,
      style = style,
      class = class
    )
  )
}


#' Add external Resources to the Application
#' 
#' This \ is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- \() {
  add_resource_path(
    'www', app_sys('app/www')
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'calculator'
    )
  )
}

