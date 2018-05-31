#' Retrieving and managing accounts
#'
#' These functions are intended to retrieve Accounts
#' @param mtoken the Monzo API auth token
#' @keywords accounts
#' @import httr jsonlite
#' @export
getAccounts <- function(mtoken = getMonzoToken()) {
    accountRequest <- GET("https://api.monzo.com/accounts", config(token = mtoken))
    accountsJson = fromJSON(content(accountRequest, type = "text"))
    accountsJson
}

#' Retrieve the first account ID for a valid account
#' @param mtoken The Monzo API auth token
#' @keywords accountId
#' @import httr jsonlite
#' @export
getDefaultAccountId <- function(mtoken = getMonzoToken()) {
    accountsResponse <- getAccounts()
    for (i in nrow(accountsResponse$accounts)) {
        account = accountsResponse$accounts[i,]
        if (!account$closed) {
            accountId <- account$id
            break;
        }
    }
    accountId
}
