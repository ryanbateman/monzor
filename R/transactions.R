#' Retrieving transactions and including items in the feed
#'
#' Retrieve transactions, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @param before An RFC 3339 encoded-timestamp
#' @param since An RFC 3339 encoded-timestamp
#' @keywords transactions
#' @import httr jsonlite
#' @export
getTransactions <- function(mtoken = getMonzoToken(), accountId = NULL, since = NULL, before = NULL, parse = TRUE) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    querylist <- list(account_id = accountId)
    if (!is.null(since)) {
        querylist$since = since
    }
    if (!is.null(before)) {
        querylist$before = before
    }
    transactionsRequest <- GET("https://api.monzo.com/transactions", config(token = mtoken), query = querylist)
    transactions = fromJSON(content(transactionsRequest, type = "text"))
    if (parse) {
        transactions <- transactions$transactions
        transactions <- parseTransactions(transactions)
    }
    transactions
}

#' Retrieve a specific transaction, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account that you're requesting transactions from
#' @param transactionId The id of the transaction being requested
#' @param expand A string dictating what should be expanded within the responses. Defaults to 'merchant'
#' @keywords transactions
#' @import httr jsonlite
#' @export
getTransaction <- function(mtoken = getMonzoToken(), accountId = NULL, transactionId = NULL, expand = "merchant", parse = TRUE) {
    stopifnot(is.character(transactionId))
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    transactionRequest <- GET(paste("https://api.monzo.com/transactions/", transactionId, sep = ""), config(token = mtoken), query = list(account_id = accountId, "expand[]" = expand))
                               transaction = fromJSON(content(transactionRequest, type = "text"))
                               if (parse) {
                                   transactions <- transaction$transaction
                                   transaction <- parseTransactions(transactions)
                               }
                               transaction
}

parseTransactions <- function(transactions = NULL) {
    transactions$created <- as.POSIXct(transactions$created, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    transactions$settled <- as.POSIXct(transactions$settled, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    transactions$updated <- as.POSIXct(transactions$updated, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    transactions$category <- as.factor(transactions$category)
    transactions$scheme <- as.factor(transactions$scheme)

    transactions
}
