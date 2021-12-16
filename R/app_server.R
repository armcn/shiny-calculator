#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- \(input, output, session) {


  
  # ---- MODEL ----

  
  default_operation <- \(a, b) b
  
  
  init <- \() list(
    inputs = list(),
    display = "0",
    last_value = "",
    current_value = "0",
    operation = default_operation,
    result = 0,
    append = FALSE,
    decimal = FALSE,
    cleared = TRUE
  )
  
  
  model <- reactiveVal(init())
  
  

  # ---- UPDATE ----

  
  observe(update_number(0)) |> bindEvent(input$zero)

  
  observe(update_number(1)) |> bindEvent(input$one)

  
  observe(update_number(2)) |> bindEvent(input$two)

  
  observe(update_number(3)) |> bindEvent(input$three)

  
  observe(update_number(4)) |> bindEvent(input$four)

  
  observe(update_number(5)) |> bindEvent(input$five)

  
  observe(update_number(6)) |> bindEvent(input$six)

  
  observe(update_number(7)) |> bindEvent(input$seven)

  
  observe(update_number(8)) |> bindEvent(input$eight)

  
  observe(update_number(9)) |> bindEvent(input$nine)

  
  observe(add()) |> bindEvent(input$add)

  
  observe(subtract()) |> bindEvent(input$subtract)

  
  observe(multiply()) |> bindEvent(input$multiply)

  
  observe(divide()) |> bindEvent(input$divide)

  
  observe(decimal()) |> bindEvent(input$decimal)

  
  observe(percent()) |> bindEvent(input$percent)

  
  observe(plus_minus()) |> bindEvent(input$plus_minus)

  
  observe(equals()) |> bindEvent(input$equals)

  
  observe(clear()) |> bindEvent(input$clear)


  update_number <- \(number) {
    current_value <-
      new_current_value(number)

    inputs <-
      new_inputs(current_value)

    display <-
      new_display(inputs)

    result <-
      model()$operation(
        as.double(model()$last_value),
        as.double(current_value)
      )

    if (display_is_too_long(display)) 
      return()
    
    else 
      model() |>
        purrr::list_modify(
          current_value = current_value,
          inputs = inputs,
          display = display,
          result = result,
          append = TRUE,
          decimal = FALSE,
          cleared = FALSE
        ) |>
        model()
  }


  new_current_value <- \(number) {
    number_string <-
      as.character(number)

    current_value <-
      model()$current_value

    if (model()$decimal) 
      stringr::str_glue("{current_value}.{number_string}")
    
    else if (model()$append & current_value != "0") 
      stringr::str_c(current_value, number_string)
    
    else 
      number_string
  }


  new_inputs <- \(current_value) 
    if (model()$append || model()$decimal) 
      replace_last(current_value, model()$inputs)
    
    else 
      c(model()$inputs, current_value)


  replace_last <- \(replacement, x) 
    if (length(x) == 0) 
      replacement
  
    else 
      replace(x, length(x), replacement)


  new_display <- \(inputs) 
    if (length(inputs) == 0) 
      "0"
  
    else 
      stringr::str_c(inputs, collapse = " ")


  display_is_too_long <- \(display) 
    stringr::str_length(display) > 11


  add <- \() 
    update_operation("+", `+`)
  

  subtract <- \() 
    update_operation("-", `-`)


  multiply <- \() 
    update_operation("\U00D7", `*`)

  
  divide <- \() 
    update_operation("\U00F7", `/`)

  
  update_operation <- \(id, operation) {
    last_value <-
      as.character(model()$result)

    inputs <-
      c(model()$inputs, id)

    display <-
      new_display(inputs)

    last_input_was_operator <-
      model()$current_value == ""

    if (model()$cleared ||
        last_input_was_operator ||
        display_is_too_long(display)) 
      return()
    
    else 
      model() |>
        purrr::list_modify(
          operation = operation,
          last_value = last_value,
          current_value = "",
          inputs = inputs,
          display = display,
          append = FALSE,
          decimal = FALSE
        ) |>
        model()
  }

  
  decimal <- \() 
    if (stringr::str_detect(model()$current_value, "\\.")) 
      model() |>
        purrr::list_modify(decimal = FALSE) |>
        model()
  
    else 
      model() |>
        purrr::list_modify(decimal = TRUE) |>
        model()

  
  percent <- \() 
    modify_current_value(\(a) a / 100)

  
  plus_minus <- \() 
    modify_current_value(\(a) a * -1)

  
  modify_current_value <- \(fn) 
    if (model()$cleared) 
      return()
  
    else {
      current_value <-
        model()$current_value |>
        as.double() |>
        fn() |>
        as.character()

      inputs <-
        replace_last(current_value, model()$inputs)

      display <-
        new_display(inputs)

      result <-
        model()$operation(
          as.double(model()$last_value),
          as.double(current_value)
        )

      model() |>
        purrr::list_modify(
          current_value = current_value,
          inputs = inputs,
          display = display,
          result = result
        ) |>
        model()
    }

  
  equals <- \() 
    if (model()$cleared) 
      return()
  
    else {
      result_string <-
        round_result(model()$result)

      inputs <-
        list(result_string)

      model() |>
        purrr::list_modify(
          inputs = NULL
        ) |>
        purrr::list_modify(
          inputs = inputs,
          display = result_string,
          last_value = "",
          current_value = result_string,
          operation = default_operation
        ) |>
        model()
    }

  
  round_result <- \(result) {
    rounded <-
      as.character(round(result, 2))

    if (stringr::str_detect(rounded, "\\.00")) 
      as.character(round(result))
    
    else 
      rounded
  }

  
  clear <- \() 
    model(init())

  
  
  # ---- VIEW ----

  
  screen_text <- reactive(
    if (length(model()$inputs) <= 2) 
      ""
    
    else 
      round_result(model()$result)
  )
  

  output$upper_screen <- renderUI(
    p(model()$display, class = "upper-screen-text")
  )

  
  output$lower_screen <- renderUI(
    p(screen_text(), class = "lower-screen-text")
  )
}
