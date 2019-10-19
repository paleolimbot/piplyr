
test_that("connect function works", {
  withr::with_envvar(list(R_PI_ENDPOINT = ""), {
    expect_error(pi_connect(), "`endpoint` must be specified")
  })
  con <- pi_connect_httpbin()
  expect_is(con, "pi_connection")
  expect_identical(expect_output(print(con), "pi_connection"), con)

  con_auth <- withr::with_envvar(list(R_PI_USER = "user", R_PI_PASSWORD = "pass"), {
    pi_connect_httpbin()
  })
  expect_identical(expect_output(print(con_auth), "pi_connection"), con_auth)
})

test_that("authenticate function works", {
  withr::with_envvar(list("R_PI_PASSWORD" = ""), {
    expect_error(pi_autheticate("test_user"), "No RStudio API found")
  })
  expect_identical(pi_autheticate(""), list())
  expect_identical(pi_autheticate(NULL), list())
  withr::with_envvar(list("R_PI_PASSWORD" = "pass"), {
    expect_is(pi_autheticate("test_user"), "request")
  })
})

test_that("pi_get can be verbose", {
  skip_if_offline()

  con <- pi_connect_httpbin()
  expect_message(pi_get(con, "get", .quiet = FALSE), "^GET")
})

test_that("pi_get function works without auth", {
  skip_if_offline()

  con <- pi_connect_httpbin()
  expect_identical(pi_get(con, "get")$url, "https://httpbin.org/get")
})

test_that("pi_get function works with auth", {
  skip_if_offline()

  withr::with_envvar(list(R_PI_USER = ""), {
    con <- pi_connect_httpbin()
    expect_error(pi_get(con, "basic-auth/user/passwd"), class = "http_401")
  })

  withr::with_envvar(list(R_PI_USER = "user", R_PI_PASSWORD = "passwd"), {
    con <- pi_connect_httpbin()
    expect_true(pi_get(con, "basic-auth/user/passwd")$authenticated)
  })
})

test_that("pi_get works on the public endpoint", {
  skip_if_offline()

  con <- pi_connect_test()
  expect_identical(pi_get(con)$Links$Self, "https://devdata.osisoft.com/piwebapi/")
})

test_that("pi_url works", {
  skip_if_offline()

  con <- pi_connect_httpbin()
  expect_match(
    pi_url(con, "fun_name", query_key = "query_value"),
    "fun_name\\?query_key=query_value"
  )
})
