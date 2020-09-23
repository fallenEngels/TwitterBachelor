packages <- c()
for (pkg in packages) {
  if (pkg %in% rownames(installed.packages()) == FALSE)
  {install.packages(pkg)}
}
rm(packages, pkg)

{
  
  
  set.seed(2020)
  setwd("Y:\\Twitter Bachelor")
}