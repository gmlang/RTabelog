# load helper R scripts
r_path = "R"
for (fname in list.files(r_path)) source(file.path(r_path, fname))

# make output dir
output_path = "output"
city_path   = file.path(output_path, city)
shopURLs_path = file.path(city_path, "shopURLs")
dir.create(shopURLs_path, showWarnings = F, recursive = T)
