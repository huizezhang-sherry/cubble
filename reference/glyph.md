# Create glyph map with ggplot2

Create glyph map with ggplot2

## Usage

``` r
geom_glyph(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  x_major = NULL,
  x_minor = NULL,
  y_major = NULL,
  y_minor = NULL,
  x_scale = identity,
  y_scale = identity,
  polar = FALSE,
  width = ggplot2::rel(2.1),
  height = ggplot2::rel(1.8),
  global_rescale = TRUE,
  show.legend = NA,
  inherit.aes = TRUE
)

geom_glyph_line(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  x_major = NULL,
  x_minor = NULL,
  y_major = NULL,
  y_minor = NULL,
  polar = FALSE,
  width = ggplot2::rel(2.1),
  height = ggplot2::rel(2.1),
  show.legend = NA,
  inherit.aes = TRUE
)

geom_glyph_box(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  x_major = NULL,
  x_minor = NULL,
  y_major = NULL,
  y_minor = NULL,
  polar = FALSE,
  width = ggplot2::rel(2.1),
  height = ggplot2::rel(2.1),
  show.legend = NA,
  inherit.aes = TRUE
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options:

  If `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

  A `data.frame`, or other object, will override the plot data. All
  objects will be fortified to produce a data frame. See
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  for which variables will be created.

  A `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data. A `function` can be created from a `formula` (e.g.
  `~ head(.x, 10)`).

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used the override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    [`stat_count()`](https://ggplot2.tidyverse.org/reference/geom_bar.html),
    give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- x_major, x_minor, y_major, y_minor:

  The name of the variable (as a string) for the major and minor x and y
  axes. Together, each unique combination of `x_major` and `y_major`
  specifies a grid cell.

- y_scale, x_scale:

  The scaling function to be applied to each set of minor values within
  a grid cell. Defaults to
  [`identity`](https://rdrr.io/r/base/identity.html) so that no scaling
  is performed.

- polar:

  A logical of length 1, specifying whether the glyphs should be drawn
  in polar coordinates. Defaults to `FALSE`.

- height, width:

  The height and width of each glyph. Defaults to 95% of the
  [`resolution`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the data. Specify the width absolutely by supplying a numeric
  vector of length 1, or relative to the resolution of the data by using
  [`rel`](https://ggplot2.tidyverse.org/reference/element.html).

- global_rescale:

  Whether rescale is performed globally or on each individual glyph.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Value

a ggplot object

## Examples

``` r
print_p <- GGally::print_if_interactive

library(ggplot2)
# basic glyph map with reference line and box---------------
p <- ggplot(data = GGally::nasa,
       aes(x_major = long, x_minor = day,
           y_major = lat, y_minor = surftemp)) +
  geom_glyph_box() +
  geom_glyph_line() +
  geom_glyph() +
  theme_bw()
print_p(p)


# rescale on each individual glyph ---------------
p <- ggplot(data = GGally::nasa,
       aes(x_major = long, x_minor = day,
           y_major = lat, y_minor = surftemp)) +
  geom_glyph(global_rescale = FALSE)
print_p(p)


# adjust width and height with relative & absolute value ---------------
p <- ggplot() +
  geom_glyph(data = GGally::nasa,
             aes(x_major = long, x_minor = day,
                 y_major = lat, y_minor = surftemp),
                 width = rel(0.8), height = 1) +
   theme_bw()
print_p(p)


# apply a re-scaling on Y and use polar coordinate
p <-
  GGally::nasa |> 
  ggplot(aes(x_major = long, x_minor = day,
             y_major = lat, y_minor = ozone)) +
    geom_glyph_box(fill=NA) +
    geom_glyph_line() +
    geom_glyph(y_scale = GGally::range01, polar = TRUE)
print_p(p)
```
