#' sdf
#'
#' @importFrom haven read_spss
#'
#' @export
download_mics <- function(catalog) {
  tf <- tempfile()
  for(i in seq_len(nrow(catalog))) {
    # download the file
    cachaca(catalog[i, "full_url"], tf, mode = 'wb')
    unzipped_files <- unzip_warn_fail(tf, exdir = paste0(tempdir(), "/unzips"))
    for(this_zip in grep("\\.zip$" , unzipped_files, value = TRUE, ignore.case = TRUE ))
      unzipped_files <- c(unzipped_files, unzip_warn_fail(this_zip, exdir = paste0(tempdir() , "/unzips")))
    sav_files <- grep("\\.sav$", unzipped_files, ignore.case = TRUE, value = TRUE)
    if (length(sav_files) == 0) stop("zero files to import")
    for(this_sav in sav_files) {
      x <- data.frame(read_spss(this_sav))
      # convert all column names to lowercase
      names(x) <- tolower(names(x))
      catalog[i, 'case_count'] <- max(catalog[i, 'case_count'] , nrow(x), na.rm = TRUE )
      saveRDS(x, file = paste0(catalog[i, 'output_folder'], "/", gsub("\\.sav", ".rds", basename(this_sav), ignore.case = TRUE)), compress = FALSE)
    }
    # delete the temporary files
    suppressWarnings(file.remove(tf, unzipped_files))
    cat(paste0(data_name, " catalog entry ", i, " of ", nrow( catalog ), " stored in '", catalog[i, 'output_folder'], "'\r\n\n"))
  }
}
