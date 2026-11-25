# ==============================================================================
# Internal Data Catalog for rsdot
# ==============================================================================
# Returns a data.frame with information on available resources (datasets)
# for each function, including OSF IDs, filenames, and department mapping.
# ==============================================================================

#' Internal Catalog Lookup
#'
#' @param dataset Character. Dataset name (e.g. "departamentos", "provincias",
#'   "distritos", "distritos_2025", "aerodromos", "densidad_poblacional",
#'   "capitales", "centros_poblados", "red_vial_nacional", "red_vial_departamental",
#'   "red_vial_vecinal", "ipress", "locales_educativos", "servicios_educativos",
#'   "cobertura_movil", "centros_poblados_crecimiento", "tiempo_desplazamiento",
#'   "peligros_*", "riesgos_*", "densidad_*", "vab_ccpp_*", "flujos_*", "catastro_rural").
#' @return A data.frame with columns `departamento` (optional), `file_name`, `id_osf`, `pattern_shp` (optional).
#' @keywords internal
get_rsdot_catalog <- function(dataset) {
  dataset <- tolower(trimws(dataset))

  # Mapeo de alias para compatibilidad directa
  alias_map <- c(
    "areas_exposicion_inundacion"        = "peligros_areas_exposicion_inundacion",
    "exp_inundacion"                     = "peligros_areas_exposicion_inundacion",
    "areas_exposicion_movimientos_masa"  = "peligros_areas_exposicion_movimientos_masa",
    "exp_mov_masa"                       = "peligros_areas_exposicion_movimientos_masa",
    "inventario_inundaciones"            = "peligros_inventario_inundaciones",
    "puntos_criticos_inundaciones"       = "peligros_puntos_criticos_inundaciones",
    "puntos_criticos"                    = "peligros_puntos_criticos_inundaciones",
    "tramos_criticos_inundaciones"       = "peligros_tramos_criticos_inundaciones",
    "tramos_criticos"                    = "peligros_tramos_criticos_inundaciones",
    "zonas_criticas_movimientos_masa"    = "peligros_zonas_criticas_movimientos_masa",
    "zonas_criticas"                     = "peligros_zonas_criticas_movimientos_masa",
    "evaluacion_riesgos"                 = "riesgos_evaluacion_riesgos",
    "evaluacion"                         = "riesgos_evaluacion_riesgos",
    "zonas_riesgos_no_mitigable"         = "riesgos_zonas_riesgos_no_mitigable",
    "no_mitigable"                       = "riesgos_zonas_riesgos_no_mitigable"
  )
  if (dataset %in% names(alias_map)) {
    dataset <- alias_map[[dataset]]
  }

  # Vector estándar de 25 departamentos del Perú
  deps_25 <- c(
    "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
    "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
    "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA",
    "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA",
    "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
  )

  # ----------------------------------------------------------------------------
  # 1. LÍMITES CENSALES Y ADMINISTRATIVOS
  # ----------------------------------------------------------------------------
  if (dataset == "departamentos") {
    return(data.frame(
      file_name = "v_departamentos_2023.zip",
      id_osf = "grhe3",
      pattern_shp = ".*departamento.*\\.shp$",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "provincias") {
    return(data.frame(
      file_name = "v_provincias_2023.zip",
      id_osf = "t36aj",
      pattern_shp = ".*provincia.*\\.shp$",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "distritos") {
    return(data.frame(
      file_name = "v_distritos_2023.zip",
      id_osf = "j9ax8",
      pattern_shp = ".*distrito.*\\.shp$",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("distritos_2025", "distrito2025", "v_distritos_2025")) {
    return(data.frame(
      file_name = "v_distritos_2025.gpkg",
      id_osf = "6b0371acd159471c5f26c448",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "capitales") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "v_capitales_amazonas.gpkg", "v_capitales_ancash.gpkg", "v_capitales_apurimac.gpkg",
        "v_capitales_arequipa.gpkg", "v_capitales_ayacucho.gpkg", "v_capitales_cajamarca.gpkg",
        "v_capitales_callao.gpkg", "v_capitales_cusco.gpkg", "v_capitales_huancavelica.gpkg",
        "v_capitales_huanuco.gpkg", "v_capitales_ica.gpkg", "v_capitales_junin.gpkg",
        "v_capitales_la_libertad.gpkg", "v_capitales_lambayeque.gpkg", "v_capitales_lima.gpkg",
        "v_capitales_loreto.gpkg", "v_capitales_madre_de_dios.gpkg", "v_capitales_moquegua.gpkg",
        "v_capitales_pasco.gpkg", "v_capitales_piura.gpkg", "v_capitales_puno.gpkg",
        "v_capitales_san_martin.gpkg", "v_capitales_tacna.gpkg", "v_capitales_tumbes.gpkg",
        "v_capitales_ucayali.gpkg"
      ),
      id_osf = c(
        "tb4mf", "w6tcu", "9puye", "4vyg3", "kfjvr",
        "v53gu", "s8vbf", "23ba9", "zwb8y", "ugz9t",
        "d59q8", "a8squ", "ptd5g", "87cxu", "vugjf",
        "gfpuj", "rv7sh", "gyh2p", "cga8q", "mfekp",
        "m8xh3", "fye5t", "pjmce", "sy4na", "n3uzm"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("centros_poblados", "ccpp_asentamientos_dispersos")) {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "ccpp_asentamientos_dispersos_amazonas.gpkg", "ccpp_asentamientos_dispersos_ancash.gpkg",
        "ccpp_asentamientos_dispersos_apurimac.gpkg", "ccpp_asentamientos_dispersos_arequipa.gpkg",
        "ccpp_asentamientos_dispersos_ayacucho.gpkg", "ccpp_asentamientos_dispersos_cajamarca.gpkg",
        "ccpp_asentamientos_dispersos_callao.gpkg", "ccpp_asentamientos_dispersos_cusco.gpkg",
        "ccpp_asentamientos_dispersos_huancavelica.gpkg", "ccpp_asentamientos_dispersos_huanuco.gpkg",
        "ccpp_asentamientos_dispersos_ica.gpkg", "ccpp_asentamientos_dispersos_junin.gpkg",
        "ccpp_asentamientos_dispersos_la_libertad.gpkg", "ccpp_asentamientos_dispersos_lambayeque.gpkg",
        "ccpp_asentamientos_dispersos_lima.gpkg", "ccpp_asentamientos_dispersos_loreto.gpkg",
        "ccpp_asentamientos_dispersos_madre_de_dios.gpkg", "ccpp_asentamientos_dispersos_moquegua.gpkg",
        "ccpp_asentamientos_dispersos_pasco.gpkg", "ccpp_asentamientos_dispersos_piura.gpkg",
        "ccpp_asentamientos_dispersos_puno.gpkg", "ccpp_asentamientos_dispersos_san_martin.gpkg",
        "ccpp_asentamientos_dispersos_tacna.gpkg", "ccpp_asentamientos_dispersos_tumbes.gpkg",
        "ccpp_asentamientos_dispersos_ucayali.gpkg"
      ),
      id_osf = c(
        "wu5fq", "de456", "e89ha", "vhj7a", "xe647",
        "xrp3c", "x8b5k", "2xa5u", "btxwh", "xpw7u",
        "5wbd6", "hv8rk", "zja4k", "ftyq6", "tnhzg",
        "sufjw", "r25yv", "fsuy5", "s6bvk", "4rqcm",
        "9rp6m", "xad5r", "pbzxw", "qbx2m", "gd9ms"
      ),
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 2. INFRAESTRUCTURA DE TRANSPORTE
  # ----------------------------------------------------------------------------
  if (dataset == "aerodromos") {
    return(data.frame(
      departamento = c(
        "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
        "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN",
        "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS",
        "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN",
        "TACNA", "TUMBES", "UCAYALI"
      ),
      file_name = c(
        "aerodromo_amazonas.gpkg", "aerodromo_ancash.gpkg", "aerodromo_apurimac.gpkg",
        "aerodromo_arequipa.gpkg", "aerodromo_ayacucho.gpkg", "aerodromo_cajamarca.gpkg",
        "aerodromo_cusco.gpkg", "aerodromo_huanuco.gpkg", "aerodromo_ica.gpkg",
        "aerodromo_junin.gpkg", "aerodromo_la_libertad.gpkg", "aerodromo_lambayeque.gpkg",
        "aerodromo_lima.gpkg", "aerodromo_loreto.gpkg", "aerodromo_madre_de_dios.gpkg",
        "aerodromo_moquegua.gpkg", "aerodromo_pasco.gpkg", "aerodromo_piura.gpkg",
        "aerodromo_puno.gpkg", "aerodromo_san_martin.gpkg", "aerodromo_tacna.gpkg",
        "aerodromo_tumbes.gpkg", "aerodromo_ucayali.gpkg"
      ),
      id_osf = c(
        "3hp9y", "krb5x", "xsvdy", "cs73x", "csp9y",
        "wrfvt", "7g8sj", "8tj6v", "7cfmh", "7up4r",
        "rcpyu", "jxug3", "wrh2d", "nsb96", "bwzvm",
        "zwg8x", "57zfg", "35yrb", "etb39", "3cehq",
        "6tms5", "udskr", "gf5xt"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "red_vial_nacional") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "red_vial_nacional_amazonas.gpkg", "red_vial_nacional_ancash.gpkg",
        "red_vial_nacional_apurimac.gpkg", "red_vial_nacional_arequipa.gpkg",
        "red_vial_nacional_ayacucho.gpkg", "red_vial_nacional_cajamarca.gpkg",
        "red_vial_nacional_callao.gpkg", "red_vial_nacional_cusco.gpkg",
        "red_vial_nacional_huancavelica.gpkg", "red_vial_nacional_huanuco.gpkg",
        "red_vial_nacional_ica.gpkg", "red_vial_nacional_junin.gpkg",
        "red_vial_nacional_la_libertad.gpkg", "red_vial_nacional_lambayeque.gpkg",
        "red_vial_nacional_lima.gpkg", "red_vial_nacional_loreto.gpkg",
        "red_vial_nacional_madre_de_dios.gpkg", "red_vial_nacional_moquegua.gpkg",
        "red_vial_nacional_pasco.gpkg", "red_vial_nacional_piura.gpkg",
        "red_vial_nacional_puno.gpkg", "red_vial_nacional_san_martin.gpkg",
        "red_vial_nacional_tacna.gpkg", "red_vial_nacional_tumbes.gpkg",
        "red_vial_nacional_ucayali.gpkg"
      ),
      id_osf = c(
        "w5jt2", "n2j4d", "ngqva", "8b6nh", "wjv57",
        "j72un", "rtxk6", "8qh4b", "3bgpf", "knzjs",
        "ycdez", "mya95", "9hcsg", "ud4fk", "t6j9a",
        "qr675", "92pag", "c6f5b", "2kwsy", "54dnw",
        "9fnqp", "7w5tn", "f4bj7", "msbj5", "5ch8q"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "red_vial_departamental") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "red_vial_departamental_amazonas.gpkg", "red_vial_departamental_ancash.gpkg",
        "red_vial_departamental_apurimac.gpkg", "red_vial_departamental_arequipa.gpkg",
        "red_vial_departamental_ayacucho.gpkg", "red_vial_departamental_cajamarca.gpkg",
        "red_vial_departamental_callao.gpkg", "red_vial_departamental_cusco.gpkg",
        "red_vial_departamental_huancavelica.gpkg", "red_vial_departamental_huanuco.gpkg",
        "red_vial_departamental_ica.gpkg", "red_vial_departamental_junin.gpkg",
        "red_vial_departamental_la_libertad.gpkg", "red_vial_departamental_lambayeque.gpkg",
        "red_vial_departamental_lima.gpkg", "red_vial_departamental_loreto.gpkg",
        "red_vial_departamental_madre_de_dios.gpkg", "red_vial_departamental_moquegua.gpkg",
        "red_vial_departamental_pasco.gpkg", "red_vial_departamental_piura.gpkg",
        "red_vial_departamental_puno.gpkg", "red_vial_departamental_san_martin.gpkg",
        "red_vial_departamental_tacna.gpkg", "red_vial_departamental_tumbes.gpkg",
        "red_vial_departamental_ucayali.gpkg"
      ),
      id_osf = c(
        "f8x6v", "5g39z", "yc3dn", "tf3pa", "fu9aj",
        "e56sn", "dtej3", "dn6ta", "rcyjd", "5pv8c",
        "42hf8", "mrb64", "8jfq9", "mdq8x", "x9u4f",
        "5zsp6", "evnw9", "2auqc", "kdht6", "cvn8d",
        "9de73", "9fsy2", "zv96x", "y3aew", "pnxjf"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "red_vial_vecinal") {
    deps_vecinal <- c(
      "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
      "CAJAMARCA", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
      "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO",
      "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO",
      "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
    )
    return(data.frame(
      departamento = deps_vecinal,
      file_name = c(
        "red_vial_vecinal_amazonas.gpkg", "red_vial_vecinal_ancash.gpkg",
        "red_vial_vecinal_apurimac.gpkg", "red_vial_vecinal_arequipa.gpkg",
        "red_vial_vecinal_ayacucho.gpkg", "red_vial_vecinal_cajamarca.gpkg",
        "red_vial_vecinal_cusco.gpkg", "red_vial_vecinal_huancavelica.gpkg",
        "red_vial_vecinal_huanuco.gpkg", "red_vial_vecinal_ica.gpkg",
        "red_vial_vecinal_junin.gpkg", "red_vial_vecinal_la_libertad.gpkg",
        "red_vial_vecinal_lambayeque.gpkg", "red_vial_vecinal_lima.gpkg",
        "red_vial_vecinal_loreto.gpkg", "red_vial_vecinal_madre_de_dios.gpkg",
        "red_vial_vecinal_moquegua.gpkg", "red_vial_vecinal_pasco.gpkg",
        "red_vial_vecinal_piura.gpkg", "red_vial_vecinal_puno.gpkg",
        "red_vial_vecinal_san_martin.gpkg", "red_vial_vecinal_tacna.gpkg",
        "red_vial_vecinal_tumbes.gpkg", "red_vial_vecinal_ucayali.gpkg"
      ),
      id_osf = c(
        "trke8", "ctr5k", "2547d", "nhef7", "uta9p",
        "5vmx8", "cvyxg", "h3gyc", "wsz7c", "s3uat",
        "na549", "e6ru2", "htge2", "zrht5", "7gqsn",
        "82cxp", "d83r4", "p9s3n", "nfzj4", "7e3r5",
        "3bzum", "47bj3", "wefkj", "6v342"
      ),
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 3. SERVICIOS PÚBLICOS E INFRAESTRUCTURA SOCIAL
  # ----------------------------------------------------------------------------
  if (dataset == "ipress") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "instituciones_prestadoras_servicios_salud_amazonas.gpkg",
        "instituciones_prestadoras_servicios_salud_ancash.gpkg",
        "instituciones_prestadoras_servicios_salud_apurimac.gpkg",
        "instituciones_prestadoras_servicios_salud_arequipa.gpkg",
        "instituciones_prestadoras_servicios_salud_ayacucho.gpkg",
        "instituciones_prestadoras_servicios_salud_cajamarca.gpkg",
        "instituciones_prestadoras_servicios_salud_callao.gpkg",
        "instituciones_prestadoras_servicios_salud_cusco.gpkg",
        "instituciones_prestadoras_servicios_salud_huancavelica.gpkg",
        "instituciones_prestadoras_servicios_salud_huanuco.gpkg",
        "instituciones_prestadoras_servicios_salud_ica.gpkg",
        "instituciones_prestadoras_servicios_salud_junin.gpkg",
        "instituciones_prestadoras_servicios_salud_la_libertad.gpkg",
        "instituciones_prestadoras_servicios_salud_lambayeque.gpkg",
        "instituciones_prestadoras_servicios_salud_lima.gpkg",
        "instituciones_prestadoras_servicios_salud_loreto.gpkg",
        "instituciones_prestadoras_servicios_salud_madre_de_dios.gpkg",
        "instituciones_prestadoras_servicios_salud_moquegua.gpkg",
        "instituciones_prestadoras_servicios_salud_pasco.gpkg",
        "instituciones_prestadoras_servicios_salud_piura.gpkg",
        "instituciones_prestadoras_servicios_salud_puno.gpkg",
        "instituciones_prestadoras_servicios_salud_san_martin.gpkg",
        "instituciones_prestadoras_servicios_salud_tacna.gpkg",
        "instituciones_prestadoras_servicios_salud_tumbes.gpkg",
        "instituciones_prestadoras_servicios_salud_ucayali.gpkg"
      ),
      id_osf = c(
        "3b85s", "b9qap", "zvjwb", "8xydq", "zmjgx",
        "wk632", "ky3av", "pzfsx", "s768q", "c6w3j",
        "w9j8u", "gj95n", "28m7j", "4rt9x", "ngvu9",
        "2dy5w", "gya9x", "y5pma", "de8y3", "evxb5",
        "azmq2", "my4wp", "uenj3", "br4fv", "tg4vh"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "locales_educativos") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "locales_educativos_amazonas.gpkg", "locales_educativos_ancash.gpkg",
        "locales_educativos_apurimac.gpkg", "locales_educativos_arequipa.gpkg",
        "locales_educativos_ayacucho.gpkg", "locales_educativos_cajamarca.gpkg",
        "locales_educativos_callao.gpkg", "locales_educativos_cusco.gpkg",
        "locales_educativos_huancavelica.gpkg", "locales_educativos_huanuco.gpkg",
        "locales_educativos_ica.gpkg", "locales_educativos_junin.gpkg",
        "locales_educativos_la_libertad.gpkg", "locales_educativos_lambayeque.gpkg",
        "locales_educativos_lima.gpkg", "locales_educativos_loreto.gpkg",
        "locales_educativos_madre_de_dios.gpkg", "locales_educativos_moquegua.gpkg",
        "locales_educativos_pasco.gpkg", "locales_educativos_piura.gpkg",
        "locales_educativos_puno.gpkg", "locales_educativos_san_martin.gpkg",
        "locales_educativos_tacna.gpkg", "locales_educativos_tumbes.gpkg",
        "locales_educativos_ucayali.gpkg"
      ),
      id_osf = c(
        "k9rsb", "5zams", "2p96k", "syq4r", "cqnb8",
        "rxmyz", "suh8t", "zx47g", "pv4ms", "afkgz",
        "6fhsn", "2ejaz", "xvpcw", "cresx", "cpz9t",
        "d5b8e", "n2cmg", "phkda", "paqdv", "2mh5y",
        "5zg68", "7625d", "zurp3", "exuty", "wr3cj"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset == "servicios_educativos") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "servicios_educativos_amazonas.gpkg", "servicios_educativos_ancash.gpkg",
        "servicios_educativos_apurimac.gpkg", "servicios_educativos_arequipa.gpkg",
        "servicios_educativos_ayacucho.gpkg", "servicios_educativos_cajamarca.gpkg",
        "servicios_educativos_callao.gpkg", "servicios_educativos_cusco.gpkg",
        "servicios_educativos_huancavelica.gpkg", "servicios_educativos_huanuco.gpkg",
        "servicios_educativos_ica.gpkg", "servicios_educativos_junin.gpkg",
        "servicios_educativos_la_libertad.gpkg", "servicios_educativos_lambayeque.gpkg",
        "servicios_educativos_lima.gpkg", "servicios_educativos_loreto.gpkg",
        "servicios_educativos_madre_de_dios.gpkg", "servicios_educativos_moquegua.gpkg",
        "servicios_educativos_pasco.gpkg", "servicios_educativos_piura.gpkg",
        "servicios_educativos_puno.gpkg", "servicios_educativos_san_martin.gpkg",
        "servicios_educativos_tacna.gpkg", "servicios_educativos_tumbes.gpkg",
        "servicios_educativos_ucayali.gpkg"
      ),
      id_osf = c(
        "zv64n", "wy42d", "kgq2p", "sujwm", "y8rnk",
        "wfc8b", "ndcyf", "9nkeb", "kebgs", "nb3cu",
        "v9g8d", "q76fj", "54hek", "6b9tm", "csyxd",
        "bv4t7", "xe5sj", "vmzfg", "9w2ne", "ywdgc",
        "s5mvt", "tk842", "qrdfb", "mj5gf", "38qaw"
      ),
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 4. TELECOMUNICACIONES
  # ----------------------------------------------------------------------------
  if (dataset %in% c("cobertura_movil", "cobertura_movil_c")) {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "cobertura_movil_oper_amazonas.gpkg", "cobertura_movil_oper_ancash.gpkg",
        "cobertura_movil_oper_apurimac.gpkg", "cobertura_movil_oper_arequipa.gpkg",
        "cobertura_movil_oper_ayacucho.gpkg", "cobertura_movil_oper_cajamarca.gpkg",
        "cobertura_movil_oper_callao.gpkg", "cobertura_movil_oper_cusco.gpkg",
        "cobertura_movil_oper_huancavelica.gpkg", "cobertura_movil_oper_huanuco.gpkg",
        "cobertura_movil_oper_ica.gpkg", "cobertura_movil_oper_junin.gpkg",
        "cobertura_movil_oper_la_libertad.gpkg", "cobertura_movil_oper_lambayeque.gpkg",
        "cobertura_movil_oper_lima.gpkg", "cobertura_movil_oper_loreto.gpkg",
        "cobertura_movil_oper_madre_de_dios.gpkg", "cobertura_movil_oper_moquegua.gpkg",
        "cobertura_movil_oper_pasco.gpkg", "cobertura_movil_oper_piura.gpkg",
        "cobertura_movil_oper_puno.gpkg", "cobertura_movil_oper_san_martin.gpkg",
        "cobertura_movil_oper_tacna.gpkg", "cobertura_movil_oper_tumbes.gpkg",
        "cobertura_movil_oper_ucayali.gpkg"
      ),
      id_osf = c(
        "hzkc6", "rekvf", "ga2wx", "dpw8j", "3mvy4",
        "35p9r", "ak9h2", "q5xpd", "m6s74", "h7xy4",
        "jyc82", "cpgwx", "p9382", "2hy3f", "scbvw",
        "us9xq", "byqc9", "gh5fs", "dxv9c", "sz3p4",
        "d2tpj", "fbp7a", "p2mvj", "2jbkp", "vkxjf"
      ),
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 5. DEMOGRAFÍA Y MODELAMIENTO TERRITORIAL
  # ----------------------------------------------------------------------------
  if (dataset == "densidad_poblacional") {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "densidad_crecimiento_poblacional_amazonas.gpkg",
        "densidad_crecimiento_poblacional_ancash.gpkg",
        "densidad_crecimiento_poblacional_apurimac.gpkg",
        "densidad_crecimiento_poblacional_arequipa.gpkg",
        "densidad_crecimiento_poblacional_ayacucho.gpkg",
        "densidad_crecimiento_poblacional_cajamarca.gpkg",
        "densidad_crecimiento_poblacional_callao.gpkg",
        "densidad_crecimiento_poblacional_cusco.gpkg",
        "densidad_crecimiento_poblacional_huancavelica.gpkg",
        "densidad_crecimiento_poblacional_huanuco.gpkg",
        "densidad_crecimiento_poblacional_ica.gpkg",
        "densidad_crecimiento_poblacional_junin.gpkg",
        "densidad_crecimiento_poblacional_la_libertad.gpkg",
        "densidad_crecimiento_poblacional_lambayeque.gpkg",
        "densidad_crecimiento_poblacional_lima.gpkg",
        "densidad_crecimiento_poblacional_loreto.gpkg",
        "densidad_crecimiento_poblacional_madre_de_dios.gpkg",
        "densidad_crecimiento_poblacional_moquegua.gpkg",
        "densidad_crecimiento_poblacional_pasco.gpkg",
        "densidad_crecimiento_poblacional_piura.gpkg",
        "densidad_crecimiento_poblacional_puno.gpkg",
        "densidad_crecimiento_poblacional_san_martin.gpkg",
        "densidad_crecimiento_poblacional_tacna.gpkg",
        "densidad_crecimiento_poblacional_tumbes.gpkg",
        "densidad_crecimiento_poblacional_ucayali.gpkg"
      ),
      id_osf = c(
        "tkur9", "ys832", "4h76w", "n36pk", "w2yhr",
        "86hrj", "wz3xs", "dkf83", "4yzxa", "4b2vm",
        "8cpqf", "qbez7", "nyjkc", "vmhyc", "rqdbg",
        "v3sqr", "j6k3z", "f7vn6", "em7au", "e8agr",
        "kp2d7", "k23rx", "rp4yf", "jdcnz", "74unj"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("centros_poblados_crecimiento", "ccpp_crecimiento", "densidad_crecimiento_pob_ccpp")) {
    return(data.frame(
      departamento = deps_25,
      file_name = c(
        "densidad_crecim_pobla_ccpp_amazonas.gpkg",
        "densidad_crecim_pobla_ccpp_ancash.gpkg",
        "densidad_crecim_pobla_ccpp_apurimac.gpkg",
        "densidad_crecim_pobla_ccpp_arequipa.gpkg",
        "densidad_crecim_pobla_ccpp_ayacucho.gpkg",
        "densidad_crecim_pobla_ccpp_cajamarca.gpkg",
        "densidad_crecim_pobla_ccpp_callao.gpkg",
        "densidad_crecim_pobla_ccpp_cusco.gpkg",
        "densidad_crecim_pobla_ccpp_huancavelica.gpkg",
        "densidad_crecim_pobla_ccpp_huanuco.gpkg",
        "densidad_crecim_pobla_ccpp_ica.gpkg",
        "densidad_crecim_pobla_ccpp_junin.gpkg",
        "densidad_crecim_pobla_ccpp_la_libertad.gpkg",
        "densidad_crecim_pobla_ccpp_lambayeque.gpkg",
        "densidad_crecim_pobla_ccpp_lima.gpkg",
        "densidad_crecim_pobla_ccpp_loreto.gpkg",
        "densidad_crecim_pobla_ccpp_madre_de_dios.gpkg",
        "densidad_crecim_pobla_ccpp_moquegua.gpkg",
        "densidad_crecim_pobla_ccpp_pasco.gpkg",
        "densidad_crecim_pobla_ccpp_piura.gpkg",
        "densidad_crecim_pobla_ccpp_puno.gpkg",
        "densidad_crecim_pobla_ccpp_san_martin.gpkg",
        "densidad_crecim_pobla_ccpp_tacna.gpkg",
        "densidad_crecim_pobla_ccpp_tumbes.gpkg",
        "densidad_crecim_pobla_ccpp_ucayali.gpkg"
      ),
      id_osf = c(
        "5pr47", "k63gh", "ux7ze", "53fkg", "puv8d",
        "vytuw", "8mkaw", "jpdtv", "8dmk6", "cb576",
        "gy8zp", "t76jr", "43gnq", "s3c9h", "7tpgu",
        "sdtzm", "unxy7", "dtmjc", "qfpdc", "fjmv4",
        "sfgwb", "a7jpx", "kuad6", "7hdcq", "g5fx8"
      ),
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("tiempo_desplazamiento", "tiempos_desplazamiento")) {
    deps_clean <- tolower(gsub(" ", "_", deps_25))
    mapa_tiempos <- c(
      "amazonas" = "6b0394cc257674174c2d24aa",
      "ancash" = "6b039527cad81c75b3e0bf90",
      "apurimac" = "6b03955bc18a7e82c52d261f",
      "arequipa" = "6b039598cad81c75b3e0bff7",
      "ayacucho" = "6b0395e4ec2db97e59d418f2",
      "cajamarca" = "6b039638ee4035f26ee0bffb",
      "callao" = "6b03963e92e1bd3581e0c04e",
      "cusco" = "6b039687ee4035f26ee0c186",
      "huancavelica" = "6b0396b48badedeacf2d2526",
      "huanuco" = "6b0396e808f95c66892d2671",
      "ica" = "6b0396fb3f9d8192af2d266d",
      "junin" = "6b03973b07af5a7381d41a6d",
      "la_libertad" = "6b039770a550baa20ee0c0d3",
      "lambayeque" = "6b0397946813e4cac8d418bd",
      "lima" = "6b0397eaee4035f26ee0c2ad",
      "loreto" = "6b0398106813e4cac8d41917",
      "madre_de_dios" = "6b03981deb9c5413ded418b4",
      "moquegua" = "6b03983162479696bde0bf0a",
      "pasco" = "6b0398485af13185bbe0bfe2",
      "piura" = "6b039874eb9c5413ded4190d",
      "puno" = "6b03989f44c4a240272d2479",
      "san_martin" = "6b0398b4a0feeecdd5e0bef6",
      "tacna" = "6b0398ce97a49502f02d25f0",
      "tumbes" = "6b0398d7858eaf82c32d251d",
      "ucayali" = "6b0398e944c4a240272d24c3"
    )
    return(data.frame(
      departamento = deps_25,
      file_name = paste0("tiempo_desplazamiento_capital_cercano_", deps_clean, ".gpkg"),
      id_osf = unname(mapa_tiempos[deps_clean]),
      stringsAsFactors = FALSE
    ))
  }

  # Modelos de Densidad de Actividades Económicas y Viviendas (Grillas Nacionales)
  if (dataset %in% c("densidad_comercio", "comercio")) {
    return(data.frame(
      file_name = "densidad_comercio.gpkg",
      id_osf = "6b0393495a06a06940e0bf17",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("densidad_mercados", "densidad_mercado", "mercado", "mercados")) {
    return(data.frame(
      file_name = "densidad_mercado.gpkg",
      id_osf = "6b039357c18a7e82c52d2435",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("densidad_financiero", "densidad_servicio_financiero", "financiero")) {
    return(data.frame(
      file_name = "densidad_servicio_financiero.gpkg",
      id_osf = "6b0393608db706a75fd418b9",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("densidad_inmobiliario", "inmobiliario")) {
    return(data.frame(
      file_name = "densidad_inmobiliario.gpkg",
      id_osf = "6b0393506a20e852d1d419e7",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("densidad_viviendas", "viviendas")) {
    return(data.frame(
      file_name = "densidad_viviendas.gpkg",
      id_osf = "6b03944897a49502f02d252d",
      stringsAsFactors = FALSE
    ))
  }

  # Modelos Socioeconómicos de Valor Agregado Bruto (VAB) de Centros Poblados (150m)
  if (dataset %in% c("vab_ccpp_total", "ccpp150_vab_total", "vab_total")) {
    return(data.frame(
      file_name = "ccpp150_vab_total.gpkg",
      id_osf = "6b03933c5a06a06940e0bf0e",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("vab_ccpp_lima", "ccpp150_vab_lima", "vab_lima")) {
    return(data.frame(
      file_name = "ccpp150_vab_lima.gpkg",
      id_osf = "6b039330c71bf75bd1d41996",
      stringsAsFactors = FALSE
    ))
  }

  if (dataset %in% c("vab_ccpp_callao", "ccpp150_vab_callao", "vab_callao")) {
    return(data.frame(
      file_name = "ccpp150_vab_callao.gpkg",
      id_osf = "6b039329c6c0c6ab66e0bf2d",
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 6. ANÁLISIS DE FLUJOS Y MOVILIDAD TERRITORIAL
  # ----------------------------------------------------------------------------
  if (grepl("^flujos?_est", dataset)) {
    rango <- sub("^flujos?_est_?", "", dataset)
    if (rango == "" || rango == "flujos_estudiantes") rango <- "100_1000"
        mapa_ids_est <- c(
      "100_1000" = "6b0394545a06a06940e0bfe8",
      "1000_3500" = "6b03945b63c0ffcb7fe0bf61",
      "3500_7000" = "6b0394692765ec271de0bedd",
      "7000_13000" = "6b039470a411da2c2c2d24f4",
      "13000" = "6b03946292e1bd3581e0bee8"
    )
    id_val <- if (rango %in% names(mapa_ids_est)) mapa_ids_est[[rango]] else paste0("STAGING_FLUJO_EST_", toupper(rango))
    return(data.frame(
      file_name = paste0("flujo_est_", rango, ".gpkg"),
      id_osf = id_val,
      stringsAsFactors = FALSE
    ))
  }

  if (grepl("^flujos?_trb", dataset)) {
    rango <- sub("^flujos?_trb_?", "", dataset)
    if (rango == "" || rango == "flujos_trabajadores") rango <- "100_1500"
        mapa_ids_trb <- c(
      "100_1500" = "6b03947c07af5a7381d418a8",
      "1500_5000" = "6b0394892765ec271de0bef1",
      "5000_10000" = "6b03949792e1bd3581e0bf11",
      "10000_25000" = "6b039482c60ce209f1d4192c",
      "25000" = "6b039491c71bf75bd1d41a6a"
    )
    id_val <- if (rango %in% names(mapa_ids_trb)) mapa_ids_trb[[rango]] else paste0("STAGING_FLUJO_TRB_", toupper(rango))
    return(data.frame(
      file_name = paste0("flujo_trb_", rango, ".gpkg"),
      id_osf = id_val,
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 7. ASPECTOS SOCIOCULTURALES Y CATASTRO RURAL
  # ----------------------------------------------------------------------------
  if (dataset %in% c("catastro_rural", "servicios_ogc_catastro_rural")) {
    return(data.frame(
      file_name = "servicios_ogc_catastro_rural.gpkg",
      id_osf = "STAGING_CATASTRO_RURAL",
      stringsAsFactors = FALSE
    ))
  }

  # ----------------------------------------------------------------------------
  # 8. PELIGROS Y RIESGOS
  # ----------------------------------------------------------------------------
  peligros_y_riesgos <- c(
    "peligros_areas_exposicion_inundacion",
    "peligros_areas_exposicion_movimientos_masa",
    "peligros_inventario_inundaciones",
    "peligros_puntos_criticos_inundaciones",
    "peligros_tramos_criticos_inundaciones",
    "peligros_zonas_criticas_movimientos_masa",
    "riesgos_evaluacion_riesgos",
    "riesgos_zonas_riesgos_no_mitigable"
  )

  # Tabla de resolución de IDs en OSF para Peligros y Riesgos
  catalogo_peligros_riesgos_osf <- c(
    "evaluacion_riesgos_amazonas.gpkg" = "6b0398f190e22f78452d24d1",
    "evaluacion_riesgos_ancash.gpkg" = "6b0398f61c5a2b0ec4e0bf01",
    "evaluacion_riesgos_apurimac.gpkg" = "6b0398fb6d4604298fd418e2",
    "evaluacion_riesgos_arequipa.gpkg" = "6b039900eb9c5413ded41988",
    "evaluacion_riesgos_ayacucho.gpkg" = "6b0399096813e4cac8d4194f",
    "evaluacion_riesgos_cajamarca.gpkg" = "6b03990f2d37d5276d2d24e0",
    "evaluacion_riesgos_callao.gpkg" = "6b039914216577945de0bec9",
    "evaluacion_riesgos_cusco.gpkg" = "6b03991d2d37d5276d2d24e8",
    "evaluacion_riesgos_huancavelica.gpkg" = "6b03992289fe1cb408d4199e",
    "evaluacion_riesgos_huanuco.gpkg" = "6b0399286d4604298fd41900",
    "evaluacion_riesgos_ica.gpkg" = "6b039931ee31af376fe0bf0b",
    "evaluacion_riesgos_junin.gpkg" = "6b03993844c4a240272d2502",
    "evaluacion_riesgos_la_libertad.gpkg" = "6b03994590e22f78452d250e",
    "evaluacion_riesgos_lambayeque.gpkg" = "6b03994da0feeecdd5e0bf88",
    "evaluacion_riesgos_lima.gpkg" = "6b039955a0feeecdd5e0bf91",
    "evaluacion_riesgos_loreto.gpkg" = "6b03995ca0feeecdd5e0bf97",
    "evaluacion_riesgos_madre_de_dios.gpkg" = "6b03997844c4a240272d254e",
    "evaluacion_riesgos_moquegua.gpkg" = "6b03997fbda1456298d41996",
    "evaluacion_riesgos_pasco.gpkg" = "6b039988858eaf82c32d259b",
    "evaluacion_riesgos_piura.gpkg" = "6b03998f6813e4cac8d41973",
    "evaluacion_riesgos_puno.gpkg" = "6b03999690e22f78452d2568",
    "evaluacion_riesgos_san_martin.gpkg" = "6b03999f216577945de0bf0c",
    "evaluacion_riesgos_tacna.gpkg" = "6b0399a8bda1456298d419c6",
    "evaluacion_riesgos_tumbes.gpkg" = "6b0399b22d37d5276d2d251d",
    "evaluacion_riesgos_ucayali.gpkg" = "6b0399bb90e22f78452d257f",
    "zonas_riesgos_no_mitigable_ancash.gpkg" = "6b0399c544c4a240272d2590",
    "zonas_riesgos_no_mitigable_cajamarca.gpkg" = "6b0399d044c4a240272d2595",
    "zonas_riesgos_no_mitigable_cusco.gpkg" = "6b0399da1c5a2b0ec4e0bfb0",
    "zonas_riesgos_no_mitigable_ica.gpkg" = "6b0399e43dba48531ed41880",
    "zonas_riesgos_no_mitigable_junin.gpkg" = "6b0399ed44c4a240272d25af",
    "zonas_riesgos_no_mitigable_la_libertad.gpkg" = "6b039a346d4604298fd419be",
    "zonas_riesgos_no_mitigable_lambayeque.gpkg" = "6b039a3fee31af376fe0bfc3",
    "zonas_riesgos_no_mitigable_lima.gpkg" = "6b039a4989fe1cb408d41ab6",
    "zonas_riesgos_no_mitigable_piura.gpkg" = "6b039a54216577945de0bf4a",
    "areas_exposicion_inundacion_amazonas.gpkg" = "6b039a5dee31af376fe0bfd8",
    "areas_exposicion_inundacion_ancash.gpkg" = "6b039a65e4c29e0ac8d418a2",
    "areas_exposicion_inundacion_apurimac.gpkg" = "6b039a6be4c29e0ac8d418a6",
    "areas_exposicion_inundacion_arequipa.gpkg" = "6b039a71216577945de0bf55",
    "areas_exposicion_inundacion_ayacucho.gpkg" = "6b039a778735cfece32d24ab",
    "areas_exposicion_inundacion_cajamarca.gpkg" = "6b039a7c3dba48531ed418ec",
    "areas_exposicion_inundacion_callao.gpkg" = "6b039a84fe37a25e2f2d24a2",
    "areas_exposicion_inundacion_cusco.gpkg" = "6b039a8d8735cfece32d24b5",
    "areas_exposicion_inundacion_huancavelica.gpkg" = "6b039a945af13185bbe0bfff",
    "areas_exposicion_inundacion_huanuco.gpkg" = "6b039a9efa4a489f5ee0bf3c",
    "areas_exposicion_inundacion_ica.gpkg" = "6b039aa9cc6a1b76622d24ae",
    "areas_exposicion_inundacion_junin.gpkg" = "6b039ab9fe37a25e2f2d2506",
    "areas_exposicion_inundacion_la_libertad.gpkg" = "6b039ac8cc6a1b76622d24fb",
    "areas_exposicion_inundacion_lambayeque.gpkg" = "6b039ad5fe37a25e2f2d253d",
    "areas_exposicion_inundacion_lima.gpkg" = "6b039ae1e4c29e0ac8d4196b",
    "areas_exposicion_inundacion_loreto.gpkg" = "6b039ae8e4d0635ad5e0beee",
    "areas_exposicion_inundacion_moquegua.gpkg" = "6b039af1ed55e84716d419e3",
    "areas_exposicion_inundacion_pasco.gpkg" = "6b039afbcc6a1b76622d2554",
    "areas_exposicion_inundacion_piura.gpkg" = "6b039b082c6fcdac802d25c4",
    "areas_exposicion_inundacion_puno.gpkg" = "6b039b11e4c29e0ac8d419ca",
    "areas_exposicion_inundacion_san_martin.gpkg" = "6b039b19216577945de0c02a",
    "areas_exposicion_inundacion_tacna.gpkg" = "6b039b233dba48531ed4196b",
    "areas_exposicion_inundacion_tumbes.gpkg" = "6b039b2dfe37a25e2f2d25fd",
    "areas_exposicion_inundacion_ucayali.gpkg" = "6b039b365387b1f072d41997",
    "areas_exposicion_movimientos_masa_amazonas.gpkg" = "6b039b41fa4a489f5ee0c024",
    "areas_exposicion_movimientos_masa_ancash.gpkg" = "6b039b57e4d0635ad5e0bf71",
    "areas_exposicion_movimientos_masa_apurimac.gpkg" = "6b039b63cc6a1b76622d25e0",
    "areas_exposicion_movimientos_masa_arequipa.gpkg" = "6b039b6ffe37a25e2f2d2638",
    "areas_exposicion_movimientos_masa_ayacucho.gpkg" = "6b039b7ded55e84716d41a80",
    "areas_exposicion_movimientos_masa_cajamarca.gpkg" = "6b039b89e4d0635ad5e0c04e",
    "areas_exposicion_movimientos_masa_callao.gpkg" = "6b039b92fe37a25e2f2d2668",
    "areas_exposicion_movimientos_masa_cusco.gpkg" = "6b039ba3b3ff3a58bbe0c0e1",
    "areas_exposicion_movimientos_masa_huancavelica.gpkg" = "6b039bb2ed55e84716d41ab8",
    "areas_exposicion_movimientos_masa_huanuco.gpkg" = "6b039bc1e4d0635ad5e0c082",
    "areas_exposicion_movimientos_masa_ica.gpkg" = "6b039be5e4d0635ad5e0c095",
    "areas_exposicion_movimientos_masa_junin.gpkg" = "6b039bfaed55e84716d41ae3",
    "areas_exposicion_movimientos_masa_la_libertad.gpkg" = "6b039c06d9bca38e9e2d2451",
    "areas_exposicion_movimientos_masa_lambayeque.gpkg" = "6b039c113338b1c5822d2459",
    "areas_exposicion_movimientos_masa_lima.gpkg" = "6b039c3a8735cfece32d25b9",
    "areas_exposicion_movimientos_masa_loreto.gpkg" = "6b039c473dba48531ed419ca",
    "areas_exposicion_movimientos_masa_madre_de_dios.gpkg" = "6b039c6440642109f0d418a0",
    "areas_exposicion_movimientos_masa_moquegua.gpkg" = "6b039c713dba48531ed419d1",
    "areas_exposicion_movimientos_masa_pasco.gpkg" = "6b039c80d9bca38e9e2d24c9",
    "areas_exposicion_movimientos_masa_piura.gpkg" = "6b039c94d9bca38e9e2d24d7",
    "areas_exposicion_movimientos_masa_puno.gpkg" = "6b039ca13338b1c5822d24b3",
    "areas_exposicion_movimientos_masa_san_martin.gpkg" = "6b039cedee209b1b482d2431",
    "areas_exposicion_movimientos_masa_tacna.gpkg" = "6b039d0294990e242dd41893",
    "areas_exposicion_movimientos_masa_tumbes.gpkg" = "6b039d1040642109f0d41923",
    "areas_exposicion_movimientos_masa_ucayali.gpkg" = "6b039d20ee209b1b482d2440",
    "inventario_inundaciones_amazonas.gpkg" = "6b039d3094990e242dd418ac",
    "inventario_inundaciones_ancash.gpkg" = "6b039d3fe9d7f936f0d41894",
    "inventario_inundaciones_apurimac.gpkg" = "6b039d4e534cdd79b0e0bea9",
    "inventario_inundaciones_arequipa.gpkg" = "6b039d5d534cdd79b0e0beab",
    "inventario_inundaciones_ayacucho.gpkg" = "6b039d6de9d7f936f0d418a5",
    "inventario_inundaciones_cajamarca.gpkg" = "6b039d8037c41cfa7ee0bed8",
    "inventario_inundaciones_callao.gpkg" = "6b039d92534cdd79b0e0bece",
    "inventario_inundaciones_cusco.gpkg" = "6b039da4534cdd79b0e0bed0",
    "inventario_inundaciones_huancavelica.gpkg" = "6b039db59c8b87972b2d24ae",
    "inventario_inundaciones_huanuco.gpkg" = "6b039dc637c41cfa7ee0bf05",
    "inventario_inundaciones_ica.gpkg" = "6b039e13d9bca38e9e2d2516",
    "inventario_inundaciones_junin.gpkg" = "6b039e2224da6003cc2d24b7",
    "inventario_inundaciones_la_libertad.gpkg" = "6b039e34aa5d6e134ee0bee5",
    "inventario_inundaciones_lambayeque.gpkg" = "6b039e70378a8a13f5d419d1",
    "inventario_inundaciones_lima.gpkg" = "6b039e819c8b87972b2d24d4",
    "inventario_inundaciones_loreto.gpkg" = "6b039e9ae9d7f936f0d4191f",
    "inventario_inundaciones_madre_de_dios.gpkg" = "6b039ead24da6003cc2d24e9",
    "inventario_inundaciones_moquegua.gpkg" = "6b039ec08a48292463e0bf4c",
    "inventario_inundaciones_pasco.gpkg" = "6b039ed2378a8a13f5d419e9",
    "inventario_inundaciones_piura.gpkg" = "6b039ee2378a8a13f5d419eb",
    "inventario_inundaciones_puno.gpkg" = "6b039efc8a48292463e0bf51",
    "inventario_inundaciones_san_martin.gpkg" = "6b039f0e97c71250b7e0beca",
    "inventario_inundaciones_tacna.gpkg" = "6b039f1fe43ac022cee0be90",
    "inventario_inundaciones_tumbes.gpkg" = "6b039f726d0a9fd05b2d2471",
    "inventario_inundaciones_ucayali.gpkg" = "6b039f8496d83da9cdd418d7",
    "puntos_criticos_inundaciones_amazonas.gpkg" = "6b039f96aa5d6e134ee0bf13",
    "puntos_criticos_inundaciones_ancash.gpkg" = "6b039fa9aa5d6e134ee0bf1d",
    "puntos_criticos_inundaciones_apurimac.gpkg" = "6b039fbce59e1b36272d249c",
    "puntos_criticos_inundaciones_arequipa.gpkg" = "6b039fdce43ac022cee0bf0e",
    "puntos_criticos_inundaciones_ayacucho.gpkg" = "6b039ff077ef1ab1202d2439",
    "puntos_criticos_inundaciones_cajamarca.gpkg" = "6b03a00577ef1ab1202d244a",
    "puntos_criticos_inundaciones_callao.gpkg" = "6b03a039aa5d6e134ee0bf3a",
    "puntos_criticos_inundaciones_cusco.gpkg" = "6b03a04f77ef1ab1202d246c",
    "puntos_criticos_inundaciones_huancavelica.gpkg" = "6b03a067e43ac022cee0bf4a",
    "puntos_criticos_inundaciones_huanuco.gpkg" = "6b03a07e77ef1ab1202d248e",
    "puntos_criticos_inundaciones_ica.gpkg" = "6b03a0a1a0b57bb7fbd4188b",
    "puntos_criticos_inundaciones_junin.gpkg" = "6b03a0da7d7180c950e0bece",
    "puntos_criticos_inundaciones_la_libertad.gpkg" = "6b03a12d9edf94d66ee0be9e",
    "puntos_criticos_inundaciones_lambayeque.gpkg" = "6b03a236e01244addae0befc",
    "puntos_criticos_inundaciones_lima.gpkg" = "6b03a255b4d9631ac0d418eb",
    "puntos_criticos_inundaciones_loreto.gpkg" = "6b03a26f9edf94d66ee0c01c",
    "puntos_criticos_inundaciones_madre_de_dios.gpkg" = "6b03a29cb4d9631ac0d41911",
    "puntos_criticos_inundaciones_moquegua.gpkg" = "6b03a2b57d7180c950e0bf0e",
    "puntos_criticos_inundaciones_pasco.gpkg" = "6b03a2dc3338b1c5822d24ef",
    "puntos_criticos_inundaciones_piura.gpkg" = "6b03a2f87b26f67d3b2d24a0",
    "puntos_criticos_inundaciones_puno.gpkg" = "6b03a327b790499dcfe0bf45",
    "puntos_criticos_inundaciones_san_martin.gpkg" = "6b03a3816d31320dc7e0bec3",
    "puntos_criticos_inundaciones_tacna.gpkg" = "6b03a3987b26f67d3b2d24a6",
    "puntos_criticos_inundaciones_tumbes.gpkg" = "6b03a3b2829c7a81c6e0bedc",
    "puntos_criticos_inundaciones_ucayali.gpkg" = "6b03a3d337fc77f074d418e6",
    "tramos_criticos_inundaciones_amazonas.gpkg" = "6b03a3f2650d9014022d2493",
    "tramos_criticos_inundaciones_ancash.gpkg" = "6b03a416c9093c208e2d247d",
    "tramos_criticos_inundaciones_apurimac.gpkg" = "6b03a49f32a44666b72d2431",
    "tramos_criticos_inundaciones_arequipa.gpkg" = "6b03a793a7945dfe6ca3a054",
    "tramos_criticos_inundaciones_ayacucho.gpkg" = "6b03a7a22659b73dc6de3140",
    "tramos_criticos_inundaciones_cajamarca.gpkg" = "6b03a7bdce27f9d4a92d24a6",
    "tramos_criticos_inundaciones_callao.gpkg" = "6b03a7cc2659b73dc6de3152",
    "tramos_criticos_inundaciones_cusco.gpkg" = "6b03a7dbf393cbfdded418ed",
    "tramos_criticos_inundaciones_huancavelica.gpkg" = "6b03a7ebf393cbfdded418f4",
    "tramos_criticos_inundaciones_huanuco.gpkg" = "6b03a7fd9ebd8c7aaac770cf",
    "tramos_criticos_inundaciones_ica.gpkg" = "6b03a80ee188ef7395c770d3",
    "tramos_criticos_inundaciones_junin.gpkg" = "6b03a81e32b94a9dd4de317b",
    "tramos_criticos_inundaciones_la_libertad.gpkg" = "6b03a82ff393cbfdded4191f",
    "tramos_criticos_inundaciones_lambayeque.gpkg" = "6b03a83f73ea9025eba3a0b3",
    "tramos_criticos_inundaciones_lima.gpkg" = "6b03a851ce27f9d4a92d24f6",
    "tramos_criticos_inundaciones_loreto.gpkg" = "6b03a87ea7945dfe6ca3a084",
    "tramos_criticos_inundaciones_madre_de_dios.gpkg" = "6b03a890aea275053ac770dd",
    "tramos_criticos_inundaciones_moquegua.gpkg" = "6b03a8b828ce80983fd41934",
    "tramos_criticos_inundaciones_pasco.gpkg" = "6b03a8cae3d36e7c50c77095",
    "tramos_criticos_inundaciones_piura.gpkg" = "6b03a8e728ce80983fd41982",
    "tramos_criticos_inundaciones_puno.gpkg" = "6b03a8fd73ea9025eba3a163",
    "tramos_criticos_inundaciones_san_martin.gpkg" = "6b03a9120810e1464fa3a04c",
    "tramos_criticos_inundaciones_tumbes.gpkg" = "6b03a92828ce80983fd419e3",
    "tramos_criticos_inundaciones_ucayali.gpkg" = "6b03a946fd1a5cc6c4c77093",
    "zonas_criticas_movimientos_masa_amazonas.gpkg" = "6b03a9653297115632c7714a",
    "zonas_criticas_movimientos_masa_ancash.gpkg" = "6b03a9860810e1464fa3a0d9",
    "zonas_criticas_movimientos_masa_apurimac.gpkg" = "6b03a9d5aea275053ac77231",
    "zonas_criticas_movimientos_masa_arequipa.gpkg" = "6b03aafa1391ae3063c7709e",
    "zonas_criticas_movimientos_masa_ayacucho.gpkg" = "6b03ab85b4508898dcaea7e9",
    "zonas_criticas_movimientos_masa_cajamarca.gpkg" = "6b03aba4c6d76e8ccaa3a083",
    "zonas_criticas_movimientos_masa_callao.gpkg" = "6b03abcd514e86fe69c87c43",
    "zonas_criticas_movimientos_masa_cusco.gpkg" = "6b03ac0f2af2e6d7f5a3a0cb",
    "zonas_criticas_movimientos_masa_huancavelica.gpkg" = "6b03ac8f34ba7003eda3a04a",
    "zonas_criticas_movimientos_masa_huanuco.gpkg" = "6b03ae91d3622dc50ac77094",
    "zonas_criticas_movimientos_masa_ica.gpkg" = "6b03bef391fd507e11d418be",
    "zonas_criticas_movimientos_masa_junin.gpkg" = "6b03bef7af45b80b9ad418b3",
    "zonas_criticas_movimientos_masa_la_libertad.gpkg" = "6b03befb7e4c954d60d418ba",
    "zonas_criticas_movimientos_masa_lambayeque.gpkg" = "6b03befeaf45b80b9ad418b5",
    "zonas_criticas_movimientos_masa_lima.gpkg" = "6b03bf012b1b36fedec770a7",
    "zonas_criticas_movimientos_masa_loreto.gpkg" = "6b03bf05600a96d568a3a085",
    "zonas_criticas_movimientos_masa_madre_de_dios.gpkg" = "6b03bf092b1b36fedec770ab",
    "zonas_criticas_movimientos_masa_moquegua.gpkg" = "6b03bf0c7e4c954d60d418c0",
    "zonas_criticas_movimientos_masa_pasco.gpkg" = "6b03bf0f63cbd2ed45d418b5",
    "zonas_criticas_movimientos_masa_piura.gpkg" = "6b03bf13af45b80b9ad418ba",
    "zonas_criticas_movimientos_masa_puno.gpkg" = "6b03bf1663cbd2ed45d418b7",
    "zonas_criticas_movimientos_masa_san_martin.gpkg" = "6b03bf197e4c954d60d418c4",
    "zonas_criticas_movimientos_masa_tacna.gpkg" = "6b03bf1c80b80509ada3a089",
    "zonas_criticas_movimientos_masa_tumbes.gpkg" = "6b03bf1fd2b0dec2d9c770aa",
    "zonas_criticas_movimientos_masa_ucayali.gpkg" = "6b03bf238057dd7dd4d41885"
  )

  if (dataset %in% peligros_y_riesgos) {
    prefijo <- sub("^(peligros_|riesgos_)", "", dataset)
    deps_clean <- tolower(gsub(" ", "_", deps_25))
    fnames <- paste0(prefijo, "_", deps_clean, ".gpkg")

    # Mapear IDs reales desde la tabla oficial de OSF
    ids_osf <- sapply(fnames, function(fn) {
      if (fn %in% names(catalogo_peligros_riesgos_osf)) {
        catalogo_peligros_riesgos_osf[[fn]]
      } else {
        paste0("STAGING_", toupper(sub(".gpkg$", "", fn)))
      }
    }, USE.NAMES = FALSE)

    disponibles <- !grepl("^STAGING_", ids_osf) |
                   file.exists(file.path("sdot_data/peligros/gpkg_departamentos", prefijo, fnames)) |
                   file.exists(file.path("sdot_data/riesgos/gpkg_departamentos", prefijo, fnames))

    return(data.frame(
      departamento = deps_25[disponibles],
      file_name = fnames[disponibles],
      id_osf = ids_osf[disponibles],
      stringsAsFactors = FALSE
    ))
  }

  stop("Dataset no reconocido en el cat\u00e1logo interno: ", dataset, call. = FALSE)
}
