install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")

library(httr)
library(jsonlite)
library(dplyr)

# Define your API key
api_key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJNb3V0b24gMjAyNCIsInVzZXJJZCI6MzIzMjgsImFwcGxpY2F0aW9uTmFtZSI6Ik1vdXRvbiAyMDI0IiwiaWQiOjE3OTcsInR5cGUiOiJ1c2VyLWFwcGxpY2F0aW9uIn0sImlhdCI6MTcyNDk0MDcwNywiZXhwIjoyMDQwMzAwNzA3LCJhdWQiOiJnZnciLCJpc3MiOiJnZncifQ.TeQwgWvv0L3PgXucOi8XH7REXiO0JzbdYpZEeqYaCnIEfTBpQsAGsacWg02jJ0wIXR557-KEd0hKmbuaShwIiZYZlt2D-wcpsKeinMs4iI4ABqCf10rYfXu5LsiIcpmSaaNFQZosqHlKrUmJUAByxpScXFLvVT9IUu57ZQz1OORH4WjQWdVa0pErsj3Vc2cfQU90y4W6nJTsVzMKxxUWyITJ5tWno-6S3HlxHgy4h-oxdhs6tQj4PRlCsgN2pruLym4qMaExLLnsS3WwyzF3Q0AqugnR3b7vj5qCcEuUAhgQ6LnuVdiOV0EU7fKra9Rqp4kVi-ulobH9nKb9f4XudpA4lHqsyNu8PmSMANg8710QIwPOD-rtZVmbOqyPgQmki60LIa1spVx3IHqu35Zakvtdbs7UvfZdib4SHd74ps4r8UAVOFJnVTeh1Nn8j9XIQqxSUaltjBLkD3oyVpB_YCO_pZ0Uae16GBa2KvK4L2dwmzQNm957PYSqJ31FjaZ1"

# Define the base URLs for AIS and SAR data
ais_url <- "https://gateway.api.globalfishingwatch.org/v2/fishing-activity"
sar_url <- "https://gateway.api.globalfishingwatch.org/v1/dark-vessel-detections"

# Define the bounding box for the entire world
bbox <- "-180,-90,180,90"

# Define the start and end dates
start_date <- "2018-01-01"
end_date <- "2023-12-31"

# Create a sequence of yearly intervals
years <- seq(as.Date("2018-01-01"), as.Date("2023-01-01"), by = "year")

#Function to download data
download_gfw_data <- function(api_url, start_date, end_date, bbox, api_key) {
  params <- list(
    startDate = start_date,
    endDate = end_date,
    bbox = bbox
  )
  
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  
  response <- GET(api_url, query = params, headers)
  
  if (status_code(response) == 200) {
    data <- content(response, as = "parsed", type = "application/json")
    df <- as.data.frame(data)
    return(df)
  } else {
    stop("Failed to retrieve data. Status code: ", status_code(response))
  }
}

# Try a simpler request to test if the base API is reachable
test_api_call_ais <- function() {
  api_key <- "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImtpZEtleSJ9.eyJkYXRhIjp7Im5hbWUiOiJNb3V0b24gMjAyNCIsInVzZXJJZCI6MzIzMjgsImFwcGxpY2F0aW9uTmFtZSI6Ik1vdXRvbiAyMDI0IiwiaWQiOjE3OTcsInR5cGUiOiJ1c2VyLWFwcGxpY2F0aW9uIn0sImlhdCI6MTcyNDk0MDcwNywiZXhwIjoyMDQwMzAwNzA3LCJhdWQiOiJnZnciLCJpc3MiOiJnZncifQ.TeQwgWvv0L3PgXucOi8XH7REXiO0JzbdYpZEeqYaCnIEfTBpQsAGsacWg02jJ0wIXR557-KEd0hKmbuaShwIiZYZlt2D-wcpsKeinMs4iI4ABqCf10rYfXu5LsiIcpmSaaNFQZosqHlKrUmJUAByxpScXFLvVT9IUu57ZQz1OORH4WjQWdVa0pErsj3Vc2cfQU90y4W6nJTsVzMKxxUWyITJ5tWno-6S3HlxHgy4h-oxdhs6tQj4PRlCsgN2pruLym4qMaExLLnsS3WwyzF3Q0AqugnR3b7vj5qCcEuUAhgQ6LnuVdiOV0EU7fKra9Rqp4kVi-ulobH9nKb9f4XudpA4lHqsyNu8PmSMANg8710QIwPOD-rtZVmbOqyPgQmki60LIa1spVx3IHqu35Zakvtdbs7UvfZdib4SHd74ps4r8UAVOFJnVTeh1Nn8j9XIQqxSUaltjBLkD3oyVpB_YCO_pZ0Uae16GBa2KvK4L2dwmzQNm957PYSqJ31FjaZ1"
  url <- "https://gateway.api.globalfishingwatch.org/v3/ais-data"  # Hypothetical endpoint
  params <- list(
    startDate = "2018-01-01T00:00:00Z",
    endDate = "2018-12-31T23:59:59Z",
    bbox = "-180,-90,180,90"
  )
  headers <- add_headers(Authorization = paste("Bearer", api_key))
  
  response <- GET(url, query = params, headers)
  
  if (status_code(response) == 200) {
    return(content(response, as = "text", encoding = "UTF-8"))
  } else {
    cat("Failed to retrieve data. Status code:", status_code(response), "\n")
    cat(content(response, as = "text", encoding = "UTF-8"))
    stop()
  }
}

# Call the test function to see if it works
test_output_ais <- test_api_call_ais()
print(test_output_ais)



