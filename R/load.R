
.First.lib<-function(libname, pkgname)
{
    Sys.setenv("UDUNITS_PATH"=system.file("udunits.dat", package=pkgname))
    library.dynam("udunits", pkgname, libname)
}
