library("devtools")
# install_github("r-lib/revdepcheck", type = "source")
library("revdepcheck")

res <- revdep_check()
revdep_check_save_summary()
