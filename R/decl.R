### Define types.

#' @import magrittr


### ?

fcs_header <- def_data(
    version_id = fixed_field(n = 6, what = ascii()) %>%
        check(either(literal("FCS3.0"),
                     literal("FCS3.1")))
)
