
install.packages.aws <- function(package_name) {
  install.packages(package_name,
                   repos = "http://nexus.serlresearch.ac.uk:8081/repository/r-group")
}