#' Add items to your feed
#'
#' Add item to feed
#' @param mtoken The Monzo API token
#' @param accountId The id of the account you're requesting transactions from
#' @param title The title of the item to be displayed in the feed
#' @param itembody The body text of the item to be displayed in the feed
#' @param image_url The URL of the image/icon to be displayed alongside the item in the feed
#' @param background_colour The hex value of the colour of the background for the feed item
#' @param title_colour The hex value of the colour of the title text for the feed item
#' @param body_colour The hex value of the colour of the body text for the feed item
#' @keywords feed
#' @import httr jsonlite
#' @export
addItemToFeed <- function(mtoken = getMonzoToken(), accountId = getDefaultAccountId(), title = NULL, itembody = NULL, image_url = NULL, background_colour = NULL, title_colour = NULL, body_colour = NULL) {
    addFeedItemRequest <- POST("https://api.monzo.com/feed",
                               encode = "form",
                               config(token = mtoken),
                               query = list("account_id" = accountId, "type" = "basic"),
                               body = list("params[body]" = itembody, "params[image_url]" = image_url, "params[background_color]" = background_colour, "params[title_colour]" = title_colour, "params[body_color]" = body_colour, "params[title]" = title)
                               )
    addFeedItemRequest
}
