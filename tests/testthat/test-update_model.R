clicked <- 1

testServer(app_server, {
  # a number can't start with multiple zeros
  
  session$setInputs(zero = clicked)
  session$setInputs(zero = clicked)
  
  expect_equal(model()$display, "0")
  expect_equal(model()$result, 0)

})

testServer(app_server, {
  # a symbol before a number doesn't do anything
  
  session$setInputs(add = clicked)
  
  expect_equal(model()$display, "0")
  expect_equal(model()$result, 0)
  
})

testServer(app_server, {
  # a multi-digit number
  
  session$setInputs(one = clicked)
  session$setInputs(two = clicked)
  session$setInputs(three = clicked)
  
  expect_equal(model()$display, "123")
  expect_equal(model()$result, 123)
  
})

testServer(app_server, {
  # a number with trailing zeros
  
  session$setInputs(one = clicked)
  session$setInputs(zero = clicked)
  session$setInputs(zero = clicked)
  
  expect_equal(model()$display, "100")
  expect_equal(model()$result, 100)
  
})

testServer(app_server, {
  # a decimal number can't have multiple decimals
  
  session$setInputs(one = clicked)
  session$setInputs(decimal = clicked)
  session$setInputs(two = clicked)
  session$setInputs(decimal = clicked)
  session$setInputs(three = clicked)
  
  expect_equal(model()$display, "1.23")
  expect_equal(model()$result, 1.23)
  
})

testServer(app_server, {
  # clicking decimal button before a number
  
  session$setInputs(decimal = clicked)
  session$setInputs(nine = clicked)
  
  expect_equal(model()$display, "0.9")
  expect_equal(model()$result, 0.9)
  
})

testServer(app_server, {
  # a decimal starting with zero
  
  session$setInputs(zero = clicked)
  session$setInputs(decimal = clicked)
  session$setInputs(five = clicked)
  session$setInputs(six = clicked)
  
  expect_equal(model()$display, "0.56")
  expect_equal(model()$result, 0.56)
  
})

testServer(app_server, {
  # a decimal starting with a non-zero
  
  session$setInputs(seven = clicked)
  session$setInputs(decimal = clicked)
  session$setInputs(eight = clicked)
  session$setInputs(one = clicked)
  
  expect_equal(model()$display, "7.81")
  expect_equal(model()$result, 7.81)
  
})

testServer(app_server, {
  # a decimal with trailing zeros
  
  session$setInputs(one = clicked)
  session$setInputs(decimal = clicked)
  session$setInputs(zero = clicked)
  session$setInputs(zero = clicked)
  
  expect_equal(model()$display, "1.00")
  expect_equal(model()$result, 1)
  
})

testServer(app_server, {
  # a number and a symbol
  
  session$setInputs(one = clicked)
  session$setInputs(add = clicked)
  
  expect_equal(model()$display, "1 +")
  expect_equal(model()$result, 1)
  
})

testServer(app_server, {
  # a number, symbol, and number
  
  session$setInputs(one = clicked)
  session$setInputs(add = clicked)
  session$setInputs(two = clicked)
  
  expect_equal(model()$display, "1 + 2")
  expect_equal(model()$result, 3)
  
})

testServer(app_server, {
  # multiple numbers and symbols
  
  session$setInputs(one = clicked)
  session$setInputs(add = clicked)
  session$setInputs(two = clicked)
  session$setInputs(subtract = clicked)
  session$setInputs(nine = clicked)
  
  expect_equal(model()$display, "1 + 2 - 9")
  expect_equal(model()$result, -6)
  
})

testServer(app_server, {
  # plus/minus turns positive numbers negative
  
  session$setInputs(one = clicked)
  session$setInputs(plus_minus = clicked)
  
  expect_equal(model()$display, "-1")
  expect_equal(model()$result, -1)
  
})

testServer(app_server, {
  # plus/minus turns negative numbers positive
  
  session$setInputs(zero = clicked)
  session$setInputs(subtract = clicked)
  session$setInputs(one = clicked)
  session$setInputs(equals = clicked)
  session$setInputs(plus_minus = clicked)
  
  expect_equal(model()$display, "1")
  expect_equal(model()$result, 1)
  
})

testServer(app_server, {
  # percentage divides numbers by 100
  
  session$setInputs(four = clicked)
  session$setInputs(percent = clicked)
  
  expect_equal(model()$display, "0.04")
  expect_equal(model()$result, 0.04)
  
})

testServer(app_server, {
  # equals overwrites inputs with result
  
  session$setInputs(one = clicked)
  session$setInputs(add = clicked)
  session$setInputs(two = clicked)
  session$setInputs(equals = clicked)
  
  expect_equal(model()$display, "3")
  expect_equal(model()$result, 3)
  
})

testServer(app_server, {
  # clear returns model to initial state
  
  init_model <- model()
  
  session$setInputs(four = clicked)
  session$setInputs(divide = clicked)
  session$setInputs(two = clicked)
  session$setInputs(clear = clicked)
  
  expect_equal(model(), init_model)
  expect_equal(model(), init_model)
  
})

testServer(app_server, {
  # display truncates at 11 characters
  
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  session$setInputs(one = clicked)
  
  expect_equal(
    stringr::str_length(model()$display), 
    11
  )
  
})