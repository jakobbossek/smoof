library(devtools)
library(plot3D)

load_all(".")

fids = 1:24
iid = 1L
dimensions = 2L

# save the plot of each fn as 'bbob/{fn}.pdf'
for (fid in fids) {
  bbob.fn = makeBBOBFunction(dimensions = dimensions, fid = fid, iid = iid)
  file.name = paste("bbob/", getName(bbob.fn), "pdf", sep = ".")
  pdf(file.name)
  plot3D(bbob.fn, title = getName(bbob.fn))
  dev.off()
}
