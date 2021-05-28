test_that("google font stylesheet added", {

  library(reactable)

  table <- reactable::reactable(data.frame(col1 = c('a', 'b'),
                                           col2 = c(1, 2)),
                                style = "font-family: Lato;")

  opts <- dsvizopts::merge_dsviz_options(title_family = "Lato")

  table_with_font <- import_google_font(viz = table, opts_theme = opts$theme)

  expect_equal(table_with_font$prepend[[2]]$name, "style")
  expect_equal(table_with_font$prepend[[2]]$children[[1]], "@import url('https://fonts.googleapis.com/css?family=Lato');")

})

test_that("logo added to reactable", {

  table <- reactable::reactable(data.frame(col1 = c('a', 'b'),
                                           col2 = c(1, 2)))

  opts <- dsvizopts::merge_dsviz_options(branding_include = TRUE)


  table_with_logo <- add_logo_reactable(table = table, opts_theme = opts$theme)

  expect_equal(table_with_logo$append[[1]]$name, "img")
  expect_match(table_with_logo$append[[1]]$attribs$src, "^data:image/")

})
