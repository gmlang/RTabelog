rm(list = ls())

# user input
city = "osaka"

# set paths
r_path = "R"
for (fname in list.files(r_path)) source(file.path(r_path, fname))

