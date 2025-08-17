#' @import chromote
create_chromote_session <- function() {
  session <- chromote::ChromoteSession$new()
  session$Emulation$setUserAgentOverride(
    userAgent = paste(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
      "AppleWebKit/537.36 (KHTML, like Gecko)",
      "Chrome/124.0.0.0 Safari/537.36"
    )
  )
  session$Network$enable()
  session$Network$setExtraHTTPHeaders(headers = list(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Cache-Control" = "no-cache",
    "Pragma" = "no-cache",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = "https://stats.ncaa.org/"
  ))

  session
}

#' @import rvest
scrape_dynamic_tables <- function(url, session = NULL, pause_ms = 1000, return_as_html = TRUE) {
  if (is.null(session)) {
    delete_session <- TRUE

    session <- create_chromote_session()

  } else {
    delete_session <- FALSE
  }

  session$Page$navigate(url)

  Sys.sleep(pause_ms / 1000)

  table_success <- FALSE
  retries <- 0

  while (!table_success && retries < 5) {
    session$Runtime$evaluate('new Promise(r => {
    const sel = "table";                                // TODO: tighten this to the exact table selector
    (function check(){ document.querySelector(sel) ? r(1) : setTimeout(check, 200); })();
  })')


    doc_id   <- session$DOM$getDocument()$root$nodeId
    html <- session$DOM$getOuterHTML(nodeId = doc_id)
    html_txt <- session$DOM$getOuterHTML(nodeId = doc_id)$outerHTML

    page <- rvest::read_html(html_txt)

    tables <- page |> rvest::html_table()

    if (length(tables) > 0) {
      table_success <- TRUE
    } else {
      retries <- retries + 1
      Sys.sleep(pause_ms / 1000)
    }


  }



  if (delete_session) {
    session$close()
  }

  if (return_as_html) {
    return(page)
  } else {
    return(tables)
  }

}
