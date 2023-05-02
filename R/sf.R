#' add geometry list column to cubble_df object
#'
#' add geometry list column to cubble_df object
#' @param x object of class \code{cubble}
#' @param sfc object of class \code{sfc} (see package sf)
#' @param crs object of class \code{crs} (see package sf); if missing 'OGC:CRS84' is assumed (WGS84) and a message is emitted
#' @param silent logical; suppress message?
#' @export
#' @examples
#' climate_aus %>% add_geometry_column()
add_geometry_column = function(x, sfc = NULL, crs, silent = FALSE) {
	stopifnot(inherits(x, "cubble_df"),
			  is.null(sfc) || inherits(sfc, "sfc"),
			  missing(crs) || inherits(crs, "crs"),
			  all(c("long", "lat") %in% names(x)))
	if (! requireNamespace("sf", quietly = TRUE))
		stop("package sf required, please install it first")
	if (is.null(sfc)) {
		if (missing(crs)) {
			if (!silent)
				message("CRS missing: using OGC:CRS84 (WGS84) as default")
			crs = sf::st_crs("OGC:CRS84")
		}
		sfc = sf::st_geometry(sf::st_as_sf(x, coords = c("long", "lat"), crs = crs))
	}
	x$geometry = sfc
	x
}
