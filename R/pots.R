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

#' Deposit in a pot
#' @param mtoken The Monzo API token
#' @param accountId The source id of the account you're transferring from
#' @param amount The amount to deposit in the pot
#' @param dedupeId The id required to ensure the deposit is unique
#' @keywords pots
#' @import httr jsonlite
#' @export
depositIntoPot <- function(mtoken = getMonzoToken(), accountId = NULL, potId = NULL, amount = NULL, dedupeId = NULL) {
    if (is.null(accountId)) {
        accountId <- getDefaultAccountId(mtoken)
    }
    url = paste("https://api.monzo.com/pots/", potId, "/deposit", sep="")
    depositIntoPotRequest <- PUT(url, config(token = mtoken), body = list("source_account_id" = accountId, "amount" = amount, "dedupe_id" = dedupeId), verbose(), encode = "form")
    depositIntoPotJson = fromJSON(content(depositIntoPotRequest, type = "text"))
    depositIntoPotJson
}
