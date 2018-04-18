mtd_get_gridmet_normals <-
  function(raw_dir = "./data/gridmet/normals")
  {
    if(!file.exists(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))){
      mcor::mco_get_gridmet_normals() %>%
        readr::write_rds(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))
    }

    readr::read_rds(stringr::str_c(raw_dir,"/gridmet_normals.Rds"))

  }
