page_title <-  "Week2: The growth of spike in winter wheat"
giscus_repo <- "Illustratien/BSC_project_IPFS2023"
# Get the category from the current page title
category <- gsub("\\s+", "-", tolower(page_title))
add_giscus_comment <- function(comment_title) {
  # get the current time to use as the unique identifier for the comment
  comment_id <- format(Sys.time(), "%Y%m%d%H%M%S")
  # construct the Giscus API endpoint URL
  giscus_api_url <- paste0("https://giscus.app/api/v1/repos/", giscus_repo, "/issues")
  # set the headers and data for the API request
  headers <- c("Accept" = "application/json", "Content-Type" = "application/json")
  data <- list(title = paste0(comment_title, " ", comment_id), body = "")
  # send the API request to create a new Giscus issue (comment)
  response <- POST(url = giscus_api_url, headers = headers, body = toJSON(data))
  # print the response status code and body for debugging purposes
  print(paste0("Response status code: ", status_code(response)))
  print(content(response))
}
