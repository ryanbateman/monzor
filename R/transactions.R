#' Retrieving transactions and including items in the feed
#'
#' Retrieve transactions, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @keywords transactions
#' @import httr jsonlite
#' @export
getMonzoTransactions <- function(mtoken = getMonzoToken(), accountId = NULL) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    transactionsRequest <- GET("https://api.monzo.com/transactions", config(token = mtoken), verbose(), query = list(account_id = accountId))
    transactionsJson = fromJSON(content(transactionsRequest, type = "text"))
    transactionsJson
}
