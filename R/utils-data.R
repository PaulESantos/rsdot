# Internal Data Catalog for rsdot
#
# Returns a data.frame with information on available resources (datasets)
# for each function, including OSF IDs and filenames.
#
# @param dataset Character. Dataset name: "aerodromos", "provincias",
#   "densidad_poblacional", "departamentos", "distritos", etc.
# @return A data.frame with columns `departamento` (optional), `file_name`, `id_osf`, `pattern_shp` (optional), etc.
get_rsdot_catalog <- function(dataset) {
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

    if (dataset == "densidad_poblacional") {
        return(data.frame(
            departamento = c(
                "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
                "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
                "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA",
                "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA",
                "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
            ),
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

    stop("Dataset not recognized in internal catalog: ", dataset)
}
