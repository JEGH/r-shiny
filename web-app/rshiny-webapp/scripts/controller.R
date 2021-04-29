
source(paste0(controller_path, 'controller_config.R'), local = TRUE)

#_________________________________________________________________________________
source('scripts/controller/login.R',     local = TRUE)

source('scripts/controller/menus.R',     local = TRUE)

source('scripts/controller/inspector.R', local = TRUE)
 
source('scripts/controller/analysis.R',  local = TRUE)

source('scripts/controller/stats.R',     local = TRUE)

source(paste0(tools_path,tools_name) ,     local = TRUE) 