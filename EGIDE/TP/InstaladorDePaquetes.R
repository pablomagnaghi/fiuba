#######################################################################################################

######################################## Paquetes y librerias #########################################

#######################################################################################################

###### Funciones
installPackagesIfItIsNeeded <- function(package_name) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name, dep=TRUE);
  }
}

###### Paquetes
installPackagesIfItIsNeeded("magrittr")
installPackagesIfItIsNeeded("pander")
installPackagesIfItIsNeeded("dplyr")
installPackagesIfItIsNeeded("ggplot2")
installPackagesIfItIsNeeded("cowplot")
installPackagesIfItIsNeeded("corrplot")
installPackagesIfItIsNeeded("VIM")
installPackagesIfItIsNeeded("mice")
installPackagesIfItIsNeeded("gdata")
