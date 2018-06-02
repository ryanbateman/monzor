#' Retrieving transactions and including items in the feed
#'
#' Retrieve transactions, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @keywords transactions
#' @import httr jsonlite
#' @export
getTransactions <- function(mtoken = getMonzoToken(), accountId = NULL) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    transactionsRequest <- GET("https://api.monzo.com/transactions", config(token = mtoken), query = list(account_id = accountId))
    transactionsJson = fromJSON(content(transactionsRequest, type = "text"))
    transactionsJson
}


#' Retrieve a specific transaction, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account that you're requesting transactions from
#' @param transactionId The id of the transaction being requested
#' @keywords transactions
#' @import httr jsonlite
#' @export
getTransaction <- function(mtoken = getMonzoToken(), accountId = NULL, transactionId = NULL, expand = "merchant") {
    stopifnot(is.character(transactionId))
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    transactionRequest <- GET(paste("https://api.monzo.com/transactions/", transactionId, sep = ""), config(token = mtoken), query = list(account_id = accountId, "expand[]" = expand))
                               transactionJson = fromJSON(content(transactionRequest, type = "text"))
                               transactionJson
}
