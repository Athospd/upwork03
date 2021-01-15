test_that("viz_01_poverty_scatterplot works", {
  gg <- viz_01_poverty_scatterplot(upwork03::averages)
  expect_equal(class(gg), c("gg", "ggplot"))
})
