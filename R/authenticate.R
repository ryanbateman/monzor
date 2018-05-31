#' Authenticating against the API and retrieving auth tokens
#'
#' This function allows you to authenticate against the R API
#' @param key The client id of your Monzo application
#' @param secret The secret for your Monzo application
#' @keywords auth
#' @import httr
#' @export
getMonzoToken <- function(key = Sys.getenv("MONZOR_CLIENTID"), secret = Sys.getenv("MONZOR_SECRET")) {

    monzo_endpoint <- oauth_endpoint(
        authorize = "https://auth.monzo.com/",
        access = "https://api.monzo.com/oauth2/token")

    monzo_app <- oauth_app("monzor",
                           key,
                           secret)

    mytoken <- oauth2.0_token(monzo_endpoint, monzo_app)
    return(mytoken)
}
