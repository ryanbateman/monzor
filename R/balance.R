#' Retrieve an account's balance
#'
#' Retrieve balance, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @keywords balance
#' @import httr jsonlite
#' @export
getBalance <- function(mtoken = getMonzoToken(), accountId = NULL) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    balanceRequest <- GET("https://api.monzo.com/balance", config(token = mtoken), query = list(account_id = accountId))
    balance = fromJSON(content(balanceRequest, type = "text"))
    balance
}
