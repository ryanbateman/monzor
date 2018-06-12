context("Get our accounts")

httptest::with_mock_api({
    test_that("We can get our accounts", {
        accounts <- getAccounts(mtoken = NULL)
        expect_true(!is.null(accounts))
    })
})
