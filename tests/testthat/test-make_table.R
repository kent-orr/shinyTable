test_that("get_column_input_type works correctly", {
  
  # Test 1: column_class is "character"
  expect_equal(get_column_input_type("character"), "text")
  
  # Test 2: column_class is "factor"
  expect_equal(get_column_input_type("factor"), "text")
  
  # Test 3: column_class is "logical"
  expect_equal(get_column_input_type("logical"), "checkbox")
  
  # Test 4: column_class is "integer"
  expect_equal(get_column_input_type("integer"), "number")
  
  # Test 5: column_class is "numeric"
  expect_equal(get_column_input_type("numeric"), "number")
  
  # Test 6: column_class is "Date"
  expect_equal(get_column_input_type("Date"), "date")
  
  # Test 7: column_class is "POSIXct"
  expect_equal(get_column_input_type("POSIXct"), "datetime-local")
  
  # Test 8: column_class is "POSIXt"
  expect_equal(get_column_input_type("POSIXt"), "datetime-local")
  
  # Test 9: column_class is "times"
  expect_equal(get_column_input_type("times"), "time")
  
  # Test 10: column_class is any other class
  expect_equal(get_column_input_type("other_class"), "text")
})

test_that("generate_tags_input works correctly", {
  
  # Create a test data.frame
  x <- data.frame(a = c("some text", "other text"), 
                  b = c(TRUE, FALSE), 
                  c = c("555-55-5555", "123-45-6789"), 
                  d = c(lubridate::as_datetime("2022-01-01 12:00:00"), lubridate::as_datetime("2023-01-01 12:00:00")),
                  stringsAsFactors = FALSE)
  
  col_types <- c("text", "checkbox", "tel", "datetime-local")
  
  # Test 1: type = text
  tag1 <- generate_tags_input(col_types, x, 1, 1, "table1")
  expect_equal(tag1$attribs$type, "text")
  expect_equal(tag1$attribs$value, "some text")
  
  # Test 2: type = checkbox
  tag2 <- generate_tags_input(col_types, x, 1, 2, "table1")
  expect_equal(tag2$attribs$type, "checkbox")
  expect_equal(tag2$attribs$checked, TRUE)
  
  # Test 3: type = tel
  tag3 <- generate_tags_input(col_types, x, 1, 3, "table1")
  expect_equal(tag3$attribs$type, "tel")
  expect_equal(tag3$attribs$value, "555-55-5555")
  expect_equal(tag3$attribs$placeholder, "555-55-5555")
  expect_equal(tag3$attribs$pattern, "[0-9]{3}-[0-9]{3}-[0-9]{4}")
  
  # Test 4: type = datetime-local
  tag4 <- generate_tags_input(col_types, x, 1, 4, "table1")
  expect_equal(tag4$attribs$type, "datetime-local")
  expect_equal(tag4$attribs$value, "2022-01-01T12:00:00")
})
