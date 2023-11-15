# cubble works with sf

    Code
      make_spatial_sf(climate_mel)
    Message
      CRS missing: using OGC:CRS84 (WGS84) as default
    Output
      # cubble:   key: id [3], index: date, nested form, [sf]
      # spatial:  [144.8321, -37.98, 145.0964, -37.6655], WGS 84
      # temporal: date [date], prcp [dbl], tmax [dbl], tmin [dbl]
        id           long   lat  elev name   wmo_id ts                  geometry
        <chr>       <dbl> <dbl> <dbl> <chr>   <dbl> <list>           <POINT [°]>
      1 ASN00086038  145. -37.7  78.4 essen~  95866 <tibble> (144.9066 -37.7276)
      2 ASN00086077  145. -38.0  12.1 moora~  94870 <tibble>   (145.0964 -37.98)
      3 ASN00086282  145. -37.7 113.  melbo~  94866 <tibble> (144.8321 -37.6655)

