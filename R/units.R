# ==============================================================================
# Unit conversions
# ==============================================================================
# Internal currency of the model is metric tonnes of WET WEIGHT (t WW).
# Areal productivity rates are standardized to t C km^-2 yr^-1 and converted to
# t WW with a carbon-to-wet-weight factor (default 9; Pauly & Christensen 1995).
#
# Useful identities
#   1 g m^-2            = 1 t km^-2
#   1 g m^-2 d^-1       = 365 t km^-2 yr^-1
#   1 mmol O2 m^-2 d^-1 = 12 mg C m^-2 d^-1 / PQ = 4.38 t C km^-2 yr^-1 (PQ = 1)
#   1 kg C ha^-1 yr^-1  = 0.1 g C m^-2 yr^-1
#   1 t                 = 2204.62262 lb
# ==============================================================================

#' Pounds per metric tonne
#' @export
LB_PER_T <- 2204.62262

#' Supported areal productivity units
#'
#' @return character vector of unit codes accepted by [convert_productivity()].
#' @export
productivity_units <- function() {
  c("t_C_km2_yr", "g_C_m2_yr", "g_C_m2_d", "kg_C_ha_yr", "mg_C_m2_d",
    "mmol_O2_m2_d", "g_O2_m2_d", "g_DW_m2_d", "g_DW_m2_yr", "t_DW_km2_yr",
    "g_OM_m2_d", "t_WW_km2_yr")
}

#' Convert an areal productivity rate to t C km^-2 yr^-1
#'
#' @param value numeric productivity values.
#' @param unit character unit code (see [productivity_units()]); recycled.
#' @param pq photosynthetic quotient (mol O2 per mol C). Default 1.
#' @param dw_to_c carbon fraction of dry weight (default 0.34; seagrass, Duarte 1990;
#'   Juman (2005a, Thalassia) gives 83.9 t C / 249 t DW = 0.337).
#' @param om_to_c carbon fraction of organic matter / ash-free dry weight (default 0.45).
#' @param c_to_ww wet weight per unit carbon, only used for `t_WW_km2_yr` input.
#' @return numeric vector in t C km^-2 yr^-1.
#' @export
convert_productivity <- function(value, unit, pq = 1, dw_to_c = 0.34,
                                 om_to_c = 0.45, c_to_ww = 9) {
  unit <- rep_len(unit, length(value))
  bad <- setdiff(unique(unit), productivity_units())
  if (length(bad)) stop("Unknown productivity unit(s): ", paste(bad, collapse = ", "),
                        ". Supported: ", paste(productivity_units(), collapse = ", "))
  f <- vapply(unit, function(u) switch(u,
    t_C_km2_yr   = 1,
    g_C_m2_yr    = 1,
    g_C_m2_d     = 365,
    mg_C_m2_d    = 0.365,
    kg_C_ha_yr   = 0.1,
    mmol_O2_m2_d = 12e-3 * 365 / pq,
    g_O2_m2_d    = (12 / 32) * 365 / pq,
    g_DW_m2_d    = dw_to_c * 365,
    g_DW_m2_yr   = dw_to_c,
    t_DW_km2_yr  = dw_to_c,
    g_OM_m2_d    = om_to_c * 365,
    t_WW_km2_yr  = 1 / c_to_ww), numeric(1))
  value * unname(f)
}

#' Convert tonnes to pounds and back
#' @param x numeric
#' @export
t_to_lb <- function(x) x * LB_PER_T
#' @rdname t_to_lb
#' @export
lb_to_t <- function(x) x / LB_PER_T
