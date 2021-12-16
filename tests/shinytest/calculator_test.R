app <- ShinyDriver$new("../../")
app$snapshotInit("calculator_test")

# a number can't start with multiple zeros

app$setInputs(zero = "click")
app$setInputs(zero = "click")

app$snapshot()
app$setInputs(clear = "click")


# a symbol before a number doesn't do anything

app$setInputs(add = "click")

app$snapshot()
app$setInputs(clear = "click")


# a multi-digit number

app$setInputs(one = "click")
app$setInputs(two = "click")
app$setInputs(three = "click")

app$snapshot()
app$setInputs(clear = "click")


# a number with trailing zeros

app$setInputs(one = "click")
app$setInputs(zero = "click")
app$setInputs(zero = "click")

app$snapshot()
app$setInputs(clear = "click")


# a decimal number can't have multiple decimals

app$setInputs(one = "click")
app$setInputs(decimal = "click")
app$setInputs(two = "click")
app$setInputs(decimal = "click")
app$setInputs(three = "click")

app$snapshot()
app$setInputs(clear = "click")


# clicking decimal button before a number

app$setInputs(decimal = "click")
app$setInputs(nine = "click")

app$snapshot()
app$setInputs(clear = "click")


# a decimal starting with zero

app$setInputs(zero = "click")
app$setInputs(decimal = "click")
app$setInputs(five = "click")
app$setInputs(six = "click")

app$snapshot()
app$setInputs(clear = "click")


# a decimal starting with a non-zero

app$setInputs(seven = "click")
app$setInputs(decimal = "click")
app$setInputs(eight = "click")
app$setInputs(one = "click")

app$snapshot()
app$setInputs(clear = "click")


# a decimal with trailing zeros

app$setInputs(one = "click")
app$setInputs(decimal = "click")
app$setInputs(zero = "click")
app$setInputs(zero = "click")

app$snapshot()
app$setInputs(clear = "click")


# a number and a symbol

app$setInputs(one = "click")
app$setInputs(add = "click")

app$snapshot()
app$setInputs(clear = "click")


# a number, symbol, and number

app$setInputs(one = "click")
app$setInputs(add = "click")
app$setInputs(two = "click")

app$snapshot()
app$setInputs(clear = "click")


# multiple numbers and symbols

app$setInputs(one = "click")
app$setInputs(add = "click")
app$setInputs(two = "click")
app$setInputs(subtract = "click")
app$setInputs(nine = "click")

app$snapshot()
app$setInputs(clear = "click")


# plus/minus turns positive numbers negative

app$setInputs(one = "click")
app$setInputs(plus_minus = "click")

app$snapshot()
app$setInputs(clear = "click")


# plus/minus turns negative numbers positive

app$setInputs(zero = "click")
app$setInputs(subtract = "click")
app$setInputs(one = "click")
app$setInputs(equals = "click")
app$setInputs(plus_minus = "click")

app$snapshot()
app$setInputs(clear = "click")


# percentage divides numbers by 100

app$setInputs(four = "click")
app$setInputs(percent = "click")

app$snapshot()
app$setInputs(clear = "click")


# equals overwrites inputs with result

app$setInputs(one = "click")
app$setInputs(add = "click")
app$setInputs(two = "click")
app$setInputs(equals = "click")

app$snapshot()
app$setInputs(clear = "click")


# display truncates at 11 characters

app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")
app$setInputs(one = "click")

app$snapshot()
app$setInputs(clear = "click")

