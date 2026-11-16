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
installPackagesIfItIsNeeded("mice")
installPackagesIfItIsNeeded("magrittr")
installPackagesIfItIsNeeded("pander")
installPackagesIfItIsNeeded("dplyr")
installPackagesIfItIsNeeded("ggplot2")
installPackagesIfItIsNeeded("cowplot")
installPackagesIfItIsNeeded("corrplot")
installPackagesIfItIsNeeded("VIM")
installPackagesIfItIsNeeded("gdata")
installPackagesIfItIsNeeded("GoodmanKruskal")
installPackagesIfItIsNeeded("randomForest")
installPackagesIfItIsNeeded("ggpubr")
installPackagesIfItIsNeeded("rms")
installPackagesIfItIsNeeded("ROCR")
installPackagesIfItIsNeeded("pROC")
installPackagesIfItIsNeeded("e1071")
installPackagesIfItIsNeeded("C50")
