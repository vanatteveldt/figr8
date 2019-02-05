pkg.env = new.env()

#' Get the Figure 8 API key
#' @export
get.api.key = function() {
  if (!exists("api.key", envir=pkg.env)) stop("Please use figr8::set.api.key before accessing figr8 functions")
  get("api.key", envir = pkg.env)
}

#' Set the Figure 8 API key
#' @export
set.api.key = function(key) {
  assign("api.key", key, envir = pkg.env)
}

build_url = function(job_id, endpoint) paste0("https://api.figure-eight.com/v1/jobs/", job_id, "/", endpoint, ".json")

#' Get a single page from the Figure 8 API
#'
#' @param job_id the Job ID
#' @param endpoint the name of the page without the .json
#' @export
get_page = function(job_id, endpoint, ...) {
  url = build_url(job_id, endpoint)
  query = list(key=get.api.key(), ...)
  res = httr::GET(url, query=query)
  httr::stop_for_status(res)
  httr::content(res, as="parsed")
}

#' Get the status of a job
#'
#' @param job_id the Job ID
#' @export
status = function(job_id) get_page(job_id, "ping")

#' List all units in a job
#'
#' @param job_id the Job ID
#' @export
units = function(job_id) {
  result = list()
  page = 1
  while (T) {
    res = get_page(job_id, "units", page=page)
    if (length(res) == 0) return(dplyr::bind_rows(result, .id = ".id"))
    result = c(result, res)
    #message(paste("Page:", page, "#rows:", length(res),"total:", length(result)))
    page = page + 1
  }

}

get_df = function(row, metacols, targetcol) {
  # I don't know how the judgment IDS relate to the judgments :(. Sometimes it gives 4 ids but 3 judgments...)
  meta_df = as.data.frame(row[metacols], stringsAsFactors = F)
  df = data.frame(x = unlist(row[[targetcol]]$res), stringsAsFactors = F)
  colnames(df) = targetcol
  tidyr::crossing(meta_df, df)
}

#' Retrieve job results
#'
#' @param job_id the Job ID
#' @param metacols a vector of column names to retrieve as metadata (i.e., one per row)
#' @param targetcol the column to retrieve as coded value (i.e. one per judgment)
#' @export
results = function(job_id) {
  page = 1
  result = list()
  while (T) {
    res = get_page(job_id, "judgments", page=page)
    if (length(res) == 0) break
    result = c(result, res)
    message(paste("Getting results page ", page, " #results:", length(res), " (total now: ", length(result), ")"))
    page = page + 1
  }
  result = lapply(result, get_df, metacols="id", targetcol="sentiment")
  dplyr::bind_rows(result, .id=".id")
}

