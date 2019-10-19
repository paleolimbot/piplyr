
test_that("connect function works", {
  con <- pi_connect("http://httpbin.org/basic-auth/user/passwd")
  expect_is(con, "pi_connection")
})

test_that("authenticate function works", {
  expect_error(pi_autheticate("test_user"), "No RStudio API found")
  expect_null(pi_autheticate(""))
  expect_null(pi_autheticate(NULL))
  withr::with_envvar(list("R_PI_PASSWORD" = "pass"), {
    expect_is(pi_autheticate("test_user"), "request")
  })
})

test_that("pi_get function works without auth", {
  skip_if_offline()

  con_anonymous <- pi_connect("http://httpbin.org")
  expect_identical(pi_get(con_anonymous, "get")$url, "https://httpbin.org/get")
})

test_that("pi_get function works with auth", {
  skip_if_offline()

  con_anonymous <- pi_connect("http://httpbin.org/basic-auth")
  expect_error(pi_get(con_anonymous, "user/passwd"), class = "http_401")

  withr::with_envvar(list(R_PI_USER = "user", R_PI_PASSWORD = "passwd"), {
    con <- pi_connect("http://httpbin.org/basic-auth")
    expect_true(pi_get(con, "user/passwd")$authenticated)
  })
})
