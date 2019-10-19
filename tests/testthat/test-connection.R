
test_that("connect function works", {
  skip_if_offline()

  con <- pi_connect("http://httpbin.org/get")
  expect_is(con, "pi_connection")
})

test_that("authenticate function works", {
  expect_error(pi_autheticate("test_user"), "No RStudio API found")
  expect_identical(pi_autheticate(""), list())
  expect_identical(pi_autheticate(NULL), list())
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

  expect_error(pi_connect("http://httpbin.org/basic-auth/user/passwd"), class = "http_401")

  withr::with_envvar(list(R_PI_USER = "user", R_PI_PASSWORD = "passwd"), {
    con <- pi_connect("http://httpbin.org/basic-auth/user/passwd")
    expect_true(pi_get(con)$authenticated)
  })
})

test_that("pi_get works on the public endpoint", {
  withr::with_envvar(list(R_PI_USER = "webapiuser", R_PI_PASSWORD = "!try3.14webapi!"), {
    con <- pi_connect("https://devdata.osisoft.com/piwebapi")
    expect_identical(pi_get(con)$Links$Self, "https://devdata.osisoft.com/piwebapi/")
  })
})
