
test_that("asset servers functions work", {
  con <- pi_connect_public()
  expect_is(pi_assetserver_list(con)$Items, "list")
  expect_identical(
    pi_assetserver(con, "F1RSIRAQC7zjPUOfBqai218IAwUElTUlYx")$WebId,
    "F1RSIRAQC7zjPUOfBqai218IAwUElTUlYx"
  )
})
