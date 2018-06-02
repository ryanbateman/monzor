#' Retrieve, withdraw from, and deposit into pots
#'
#' Retrieve pots, defaulting to the first valid account if no account ID is supplied
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @keywords pots
#' @import httr jsonlite
#' @export
getPots <- function(mtoken = getMonzoToken(), accountId = NULL) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    potsRequest <- GET("https://api.monzo.com/pots", config(token = mtoken), query = list(account_id = accountId))
    potsJson = fromJSON(content(potsRequest, type = "text"))
    potsJson
}
