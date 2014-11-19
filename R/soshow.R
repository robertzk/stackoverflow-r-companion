latest_question <- function(value) {
  if (!missing(value)) {
    reg <- registry()
    reg$latest_question <- value
    registry(reg)
    value
  } else registry()$latest_question
}

#' Find one StackOverflow R post if not yet seen.
#'
#' @export
soshow <- function() {
  if (!interactive()) return()

  feed <- RCurl::getURLContent("http://stackoverflow.com/feeds/tag?tagnames=r&sort=newest")
  question_ids <- Filter(Negate(is.na),
    xpathSApply(htmlParse(content), '//link', function(link) {
      url <- xmlAttrs(link)['href']
      match <- regexec("questions/([0-9]+)", url)
      question_id <- regmatches(url, match)[[1]][2]
      as.integer(question_id)
    }))

  url <- c('http://api.stackexchange.com/2.2/questions?',
    'pagesize=5&order=desc&sort=activity&tagged=r&site=stackoverflow&filter=!9YdnSJrFY')
  url <- paste(url, collapse = '')
  questions <- content(GET(url))$items

  questions <- Filter(function(x) !x$is_answered &&
    (as.integer(Sys.time()) - x$creation_date) < 3600*2, questions)

  questions <- Filter(
    function(x) !is.element(x$question_id, registry()$seen_questions), questions)

  if (length(questions) == 0) return(NULL)
  display_question(questions[[1]]$question_id)
}

#' Peek to see whether there are any questions worth answering.
#'
#' A peek will not use API quotas.
#' @return \code{TRUE} or \code{FALSE} according as new questions appeared.
peek <- function() {
  reg <- registry()
  

  reg$peeked_questions <- 
}

#' Display an SO question in the R console.
#'
#' @param question integer. The question ID obtained from \code{soshow}.
#'   The default is \code{latest_question()}.
#' @return nothing, but the question will be displayed in the R console.
display_question <- function(question = latest_question()) {
  if (!interactive()) return()

  if (is.numeric(question)) {
    # The filter is used to get markdown_body. It was generated using
    #   http://api.stackexchange.com/docs/questions-by-ids#order=desc&sort=activity&ids=24845331&filter=!9YdnSJrFY&site=stackoverflow&run=true
    res <- content(GET(paste0('http://api.stackexchange.com/2.2/questions/', question,
      '?order=desc&sort=activity&site=stackoverflow&filter=!9YdnSJrFY')))

    body <- res$items[[1]]$body_markdown
    body <- Reduce(function(v, x) gsub(x[1], x[2], v, fixed = TRUE),
                   list(c("&lt;", "<"), c("&gt;", ">"), c("&quot;", '"')), body)
    res$items[[1]]$body_markdown <- body

    reg <- registry()
    reg$seen_questions <- c(reg$seen_questions, question)
    registry(reg)

    question <- latest_question(res)
  }

  cat(testthat:::colourise(body, 'purple'))



  
}

