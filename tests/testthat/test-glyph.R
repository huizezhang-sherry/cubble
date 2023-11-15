test_that("geom_glyph x_scale and y_scale can be functions", {
  skip_if_not_installed("vdiffr")
  library(ggplot2)

  p <-
    GGally::nasa |> 
    ggplot(aes(x_major = long, x_minor = day,
              y_major = lat, y_minor = ozone)) +
      geom_glyph_box(fill = NA) +
      geom_glyph_line()

  p1 <- p + geom_glyph(y_scale = "identity", polar = TRUE)
  p2 <- p + geom_glyph(y_scale = identity, polar = TRUE)
  p3 <- p + geom_glyph(y_scale = GGally::range01, polar = TRUE)

  vdiffr::expect_doppelganger("geom_glyph_y_scale_identity_str", p1)
  vdiffr::expect_doppelganger("geom_glyph_y_scale_identity", p2)
  vdiffr::expect_doppelganger("geom_glyph_y_scale_range01", p3)

  p1 <- p + geom_glyph(x_scale = "identity", polar = TRUE)
  p2 <- p + geom_glyph(x_scale = identity, polar = TRUE)
  p3 <- p + geom_glyph(x_scale = GGally::range01, polar = TRUE)

  vdiffr::expect_doppelganger("geom_glyph_x_scale_identity_str", p1)
  vdiffr::expect_doppelganger("geom_glyph_x_scale_identity", p2)
  vdiffr::expect_doppelganger("geom_glyph_x_scale_range01", p3)
})
