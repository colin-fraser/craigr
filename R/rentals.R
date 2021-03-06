#' List Available Rentals
#'
#' @description Get a list of housing available on craiglist using your own
#' specifications
#'
#' @param location Character vector containing the region to look in. Matches
#' the craigslist subdomain for your region.  Default is "seattle".
#' @param area Character vector containing the specific area to look in. Matches
#' the craigslist three-letter code for yoru area.  Default is "all".
#' @param base_url Optional character vector containing the craigslist base url.
#' This overwrites the \code{location} and \code{area} parameters if supplied.
#' @param detailed Logical indicating whether to return detailed search results,
#' attempting to scrape extra information from listing pages.  Detailed results
#' take longer to acquire. Default is \code{FALSE}.
#' @param output Optional character vector naming a CSV file to save to.
#' Default is no output.
#' @param max_results Numeric specifying the maximum number of results to
#' display.  Results are obtained 100 at a time, so the value will be rounded
#' to the nearest 100.  Defaults at the craigslist maximum of 2500.
#' @param query Optional character vector containing a search term to supply
#' to craiglist.
#' @param search_distance Optional numeric supplying a distance from the
#' specified postal code.
#' @param postal Optional numeric postal code specifying an area to search in.
#' @param bedrooms Optional character vector specifying the desired number of
#' bedrooms.
#' @param bathrooms Optional character vector specifying the desired number of
#' bathrooms.
#' @param min_price Optional numeric containing minimum price.
#' @param max_price Optional numeric containing maximum price.
#' @param min_sqft Optional numeric containing minimum square footage.
#' @param max_sqft Optional numeric containing maximum square footage.
#' @param has_pic Logical specifying whether listing must contain a photo.
#' Default is \code{FALSE}.
#' @param posted_today Logical specifying whether listing must have been posted
#' today. Default is \code{FALSE}.
#' @param pets_cat Logical specifying whether apartment must allow cats. Default
#' is \code{FALSE}.
#' @param pets_dog Logical specifying whether apartment must allow dogs. Default
#' is \code{FALSE}.
#'
#' @examples
#'
#' rentals(location = "seattle", area = "see", bedrooms = 5, query = "downtown")
#' @export
#'
rentals <- function(location = "seattle", area = "all", base_url = NULL,
                    detailed = FALSE, output = NULL,
                    max_results = 2500, query = NULL, postal = NULL,
                    search_distance = NULL, bedrooms = NULL, bathrooms = NULL,
                    min_price = NULL, max_price = NULL, min_sqft = NULL,
                    max_sqft = NULL, has_pic = FALSE, posted_today = FALSE,
                    pets_cat = FALSE, pets_dog = FALSE, progress_bar = TRUE) {
  ## Preliminary input checks -----
  # Generate the base url based on specified location and area and make sure
  # it exists
  if (missing("base_url")) {
    base_url <- get_base_url(location, area)
  }

  if (!RCurl::url.exists(base_url)) {
    stop("URL not found.  Please check your input and try again.")
  }

  # Check the parameters that are not NULL by default
  check_class(has_pic, "logical")
  check_class(posted_today, "logical")
  check_class(pets_cat, "logical")
  check_class(pets_dog, "logical")
  check_class(max_results, "numeric")

  ## Generate the query based on supplied parameters
  # Vector to whold query components
  queries <- c("?")

  # Generate queries while simultaneously checking that the user supplied inputs
  # of the correct class.
  if (!(missing(query))) {
    check_class(query, "character")
    queries <- c(queries, paste0("query=", query))
  }
  if (!(missing(bedrooms))) {
    check_class(bedrooms, "numeric")
    queries <- c(queries, paste0("bedrooms=", bedrooms))
  }
  if (!(missing(bathrooms))) {
    check_class(bathrooms, "numeric")
    queries <- c(queries, paste0("bathrooms=", bathrooms))
  }
  if (!(missing(min_price))) {
    check_class(min_price, "numeric")
    queries <- c(queries, paste0("min_price=", min_price))
  }
  if (!(missing(max_price))) {
    check_class(max_price, "numeric")
    queries <- c(queries, paste0("max_price=", max_price))
  }
  if (!(missing(min_sqft))) {
    check_class(min_sqft, "numeric")
    queries <- c(queries, paste0("minSqft=", min_sqft))
  }
  if (!(missing(max_sqft))) {
    check_class(max_sqft, "numeric")
    queries <- c(queries, paste0("maxSqft=", max_sqft))
  }
  if (!(missing(search_distance))) {
    check_class(search_distance, "numeric")
    queries <- c(queries, paste0("search_distance=", search_distance))
  }
  if (!(missing(postal))) {
    check_class(postal, "numeric")
    queries <- c(queries, paste0("postal=", postal))
  }
  if (has_pic) {
    queries <- c(queries, "hasPic=1")
  }
  if (posted_today) {
    queries <- c(queries, "postedToday=1")
  }
  if (pets_cat) {
    queries <- c(queries, "pets_cat=1")
  }
  if (pets_dog) {
    queries <- c(queries, "pets_dog=1")
  }

  # Add queries to url
  if (length(queries) > 1) {
    query_url <- paste0(
      base_url,
      queries[1],
      paste(queries[2:length(queries)], collapse = "&")
    )
  } else {
    query_url <- base_url
  }

  ## Scrape and clean the results of the query -----

  # How many results are available?
  tot_results <- get_num_results(query_url)
  # Use user specified max results if it is less than the num. available results
  if (max_results < tot_results) {
    tot_results <- max_results
  }
  # Calculate the number of pages to download (craigslist puts 100 results
  # per page)
  num_pages <- ceiling(tot_results / 100) - 1

  # Go through each page of craigslist using a loop
  if (progress_bar) {
    bar <- progress::progress_bar$new(total = num_pages+1)
  }
  for (i in 0:num_pages)
  {
    if (i == 0) {
      ## Nothing is appended to the URL for the first page
      all_results <- get_query(query_url)
    } else {
      ## Add page number
      if (length(queries) > 1) {
        ## If the query was not empty, add a new filter using "&"
        query_page <- paste0(query_url, "&s=", i * 100)
      } else {
        ## If the query was empty, add a new filter using "?"
        query_page <- paste0(query_url, "?s=", i * 100)
      }

      ## Get the results from the current page
      query_results <- get_query(query_page)

      ## Bind the results from each page
      all_results <- rbind(all_results, query_results)
    }
    if (progress_bar) {bar$tick()}
  }

  # Generate output file if desired
  if (!missing(output)) {
    write.csv(x = all_results, file = output)
  }

  return(all_results)
}
