test_that("google font stylesheet added", {

  library(reactable)

  table <- reactable::reactable(data.frame(col1 = c('a', 'b'),
                                           col2 = c(1, 2)),
                                style = "font-family: Lato;")

  opts <- dsvizopts::merge_dsviz_options(title = "some title for this", text_family = "Lato")

  table_with_font <- import_google_font(viz = table, opts_theme = opts$theme)

  expect_equal(table_with_font$prepend[[2]]$name, "style")
  expect_equal(table_with_font$prepend[[2]]$children[[1]], "@import url('https://fonts.googleapis.com/css?family=Lato');")


  library(lfltmagic)
  map <- lflt_choropleth_Gcd(sample_data("Gcd"), opts = opts)

  map_with_font <- import_google_font(viz = map, opts_theme = opts$theme)

})

test_that("logo added to reactable", {

  table <- reactable::reactable(data.frame(col1 = c('a', 'b'),
                                           col2 = c(1, 2)))

  opts_theme <- dsvizopts::merge_dsviz_options(branding_include = TRUE)$theme

  opts_theme1 <- dsthemer::dsthemer_get(org = "public")


  table_with_logo <- add_logo_reactable(table = table, opts_theme = opts_theme1)
  table_with_logo

  expect_equal(table_with_logo$append[[1]]$name, "img")
  expect_match(table_with_logo$append[[1]]$attribs$src, "^data:image/")

})
