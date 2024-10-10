## Planet API

## Order API
orderurl <- ""

## Search API
searchurl <- ""

## Check subscription/quota
subscription_url <- ""

#' Pretty Print JSON
#'
#' Print JSON for a query nicely
#'
#' @param x R object to be converted to JSON
#' @importFrom jsonlite toJSON
#' @export
jsonit <- function(x) {
        toJSON(x, pretty = TRUE, auto_unbox = TRUE)
}


#' Get API Authorization
#'
#' Get Planet API Key Authorization
#'
#' @importFrom httr authenticate
#' @export
#'
get_auth <- function() {
        key <-  Sys.getenv("PLANET_API_KEY")
        if(!nzchar(key)) {
                stop("key is length 0; make sure to set your API key as an environment variable named PLANET_API_KEY")
        }
        authenticate(key, "")
}

#' Check Quota
#'
#' Check Current Download Quota
#'
#' @description Check your current download quota and how much of it you have used.
#'
#' @return A data frame with columns 'quota_used' and 'pct_used'
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
plnt_quota <- function() {
        auth <- get_auth()
        r <- GET(subscription_url, auth)
        d <- suppressMessages(fromJSON(as.character(r)))
        d$pct_used <- with(d, round(100 * quota_used, 1))
        d[, c("quota_used", "pct_used")]
}


#' Search Planet API
#'
#' Post a search query to the Planet API
#'
#' @param searchq Search query represented as an R object
#'
#' @return An object containing the search results
#'
#' @importFrom httr POST upload_file
#' @importFrom utils URLencode
#' @importFrom jsonlite fromJSON write_json
#' @export
#'
plnt_search <- function(searchq) {
        auth <- get_auth()

        ## Write it out
        searchJSONfile <- paste(tempfile(), "json", sep = ".")
        write_json(searchq, searchJSONfile,
                   auto_unbox = TRUE, pretty = TRUE)

        ## Search
        search_string <- file.path(searchurl,
                                   URLencode(paste0("quick-search",
                                                    "?_sort=acquired asc")))
        POST(search_string,
             auth,
             body = upload_file(searchJSONfile))
}

#' Get All IDs from Search
#'
#' Follow _next URLs to get all IDs when spread across pages
#'
#' @param r Result from search query
#'
#' @returns a character vector of IDs
#'
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
get_all_ids <- function(r) {
        auth <- get_auth()
        result <- suppressMessages(fromJSON(as.character(r)))
        idlist <- list()
        i <- 1
        nextlink <- result$`_links`$`_next`
        while(!is.null(nextlink)) {
                idlist[[i]] <- result$features$id
                r <- GET(nextlink, auth)
                result <- suppressMessages(fromJSON(as.character(r)))
                nextlink <- result$`_links`$`_next`
                i <- i + 1
        }
        unlist(idlist, use.names = FALSE)
}




#' Order Planet API
#'
#' Post an order query to the Planet API
#'
#' @param ord R object containing the details of the order, such as filters, delivery, or notifications
#'
#' @return A list containing the order results
#'
#' @importFrom httr POST upload_file
#' @importFrom jsonlite write_json
#' @export
#'
plnt_order <- function(ord) {
        auth <- get_auth()
        orderJSONfile <- paste(tempfile(), "json", sep = ".")
        write_json(ord, orderJSONfile,
                   pretty = TRUE, auto_unbox = TRUE)

        POST(orderurl,
             auth,
             body = upload_file(orderJSONfile)
        )
}


#' Download Zip Bundle
#'
#' Download files associated with an order ID
#'
#' @param order_id the Planet order ID
#' @param ddir directory into which file should be downloaded
#'
#' @export
#' @importFrom utils download.file
#'
download_order <- function(order_id, ddir = "data") {
        out <- plnt_status(order_id, silent = TRUE)
        if(out$state != "success")
                stop("cannot download order; order state: ", out$state)
        nms <- basename(out$`_links`$results$name)
        i <- grep("manifest.json", nms, fixed = TRUE, invert = TRUE)
        if(length(i) > 1) {
                cat(nms[i], sep = "\n")
                stop("more than one file to download")
        }
        destfile <- file.path(ddir, nms[i])
        download.file(out$`_links`$results$location[i], destfile)
        invisible(destfile)
}



#' Check All Orders
#'
#' Get a table of the most recent orders made
#'
#' @return a data frame of orders containing the time created, order ID, state, and name
#' @export
#' @import tibble
#' @import dplyr
#' @importFrom httr GET
#' @importFrom lubridate ymd_hms
#' @importFrom jsonlite fromJSON
#'
check_orders <- function(n = 20L) {
        n <- as.integer(n)
        if(n > 20)
                stop("n must be <= 20")
        auth <- get_auth()
        r <- GET(orderurl, auth)
        response <- suppressMessages(fromJSON(as.character(r)))
        out <- with(response$orders, {
                tibble(created = created_on,
                       id = id,
                       state = state,
                       name = name)
        }) %>%
                mutate(created = ymd_hms(created)) %>%
                arrange(desc(created))
        head(out, n)
}

#' Check Status of an Order
#'
#' Return the status information for an order ID
#'
#' @param order_id Order ID
#' @param silent should status summary be printed?
#'
#' @import dplyr
#' @import tibble
#' @importFrom tidyr pivot_longer
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @export
#'
plnt_status <- function(order_id, silent = FALSE) {
        auth <- get_auth()
        status <- GET(paste(orderurl, order_id, sep = "/"), auth)
        out <- suppressMessages({
                status %>%
                        as.character() %>%
                        fromJSON()
        })
        showvars <- c("name", "created_on", "last_message", "state")
        if(all(showvars %in% names(out))
           && !silent) {
                summary <- out[showvars] %>%
                        as_tibble() %>%
                        rename(order_name = name) %>%
                        pivot_longer(everything())
                print(summary)
        }
        invisible(out)
}







