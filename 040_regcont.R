library(targets)
library(dplyr)
library(tarchetypes)
library(crew)

tar_option_set(packages = yaml::read_yaml("settings/packages.yaml")$packages, 
               controller = crew::crew_controller_local(workers = 2))

# tars -------
tars <- yaml::read_yaml("_targets.yaml")

# tar source -------
tar_source()

regcontSA <- tar_read(regcontSA, store = tars$setup$store)
sdm_summary <- tar_read(sdm_summary, store = tars$sdmstats$store)
iucn_summary <- tar_read(iucn_summary, store = tars$iucn$store)

# targets -------

tar_plan(

  #### RegContSum ----
  
  ## RegCont bind to sdm_summary ----
  tar_target(
    regcont_summary,
    command = {
      regcontSA <- regcontSA |> ## State RegCont
        dplyr::rename_with(.fn = janitor::make_clean_names) |>
        dplyr::rename_with(.fn = \(x) paste0("sa_", x))
      
      # bind other regcont
    }
  ),
  
  tar_target(
    pressure,
    command = {
      splevel <- dplyr::left_join(
        sdm_summary,
        iucn_summary,
        by = c("search_term" = "scientific_name")
      )
        
     regcont_summary |> 
       dplyr::left_join(splevel, by = c("sa_taxa" = "search_term"))   
    }
  )
)