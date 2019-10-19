
test_that("authenticate function works", {
  expect_error(pi_autheticate("test_user"), "No RStudio API found")
  expect_null(pi_autheticate(""))
  expect_null(pi_autheticate(NULL))
  withr::with_envvar(list("R_PI_PASSWORD" = "pass"), {
    expect_is(pi_autheticate("test_user"), "request")
  })
})
