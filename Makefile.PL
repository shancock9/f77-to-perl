use ExtUtils::MakeMaker;
WriteMakefile(
    NAME         => "Fortran::F77toPerl",
    VERSION_FROM => "lib/Fortran/F77toPerl.pm",
    (
        $] >= 5.005
        ? (
            ABSTRACT => 'translate Fortran 77 to perl',
            LICENSE => 'MIT',
            AUTHOR   => 'Steve Hancock <perltidy@perltidy.sourceforge.net>'
          )
        : ()
    ),
    ##EXE_FILES => ['bin/f2perl'],
    dist      => { COMPRESS => 'gzip', SUFFIX => 'gz' },
);
