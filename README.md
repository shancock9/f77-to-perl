## f77-to-perl

Fortran::F77toPerl

## SYNOPSIS

    Fortran::F77toPerl is a tool for translating Fortran 77 programs into perl. It's
    still in its early stages of development, but quite useful for certain
    tasks.

    Fortran::F77toPerl is free software released under the GNU General Public License --
    please see the included file "COPYING" for details.

## PREREQUISITES
    F2perl should run on perl systems as old as about 5.004 (this version
    was tested under perl 5.004_04), BUT it outputs code which requires perl
    5.6 or higher to run in most cases. In particular, the output code uses
    "our" variables and "lvalue" subroutines.

    F2perl will look for module Perl::Tidy and, if it finds it, it will use
    it to beautify its output. You can get Perl::Tidy at

    http://perltidy.sourceforge.net

    F2perl will run without Perl::Tidy, but the output won't look as good.

## INSTALLATION

    The standard Makefile.PL method should work:

     perl Makefile.PL
     make
     make test
     make install

    This will install the f2perl script and modules at or below
    Fortran::F77toPerl.

## USAGE

     f2perl infile.f

    This will produce infile.pl.

    This is the initial release, and there is no other documentation yet. To
    help offset this problem, there are a number of test files in the "f"
    directory in this distribution which illustrate its current capabilities
    and limitations.

## SIMPLE TESTS

    The "f" directory contains a number of test files which can be run. For
    example, try

     f2perl brent.f

    and then execute the perl script

     perl brent.pl

    If you have a Fortran compiler, compile and execute brent.f and compare
    the output.

## ABOUT

    I developed the initial version of f2perl in 2002 to assist in
    translating some mathematical and engineering programs to perl. It saved
    me a lot of time.  I do not plan to do any further development on it,
    but I have posted it in case it might be helpful to someone else.

    Fortran code doesn't always map well into Perl, so some human
    intervention and careful checking is almost always required to get a
    working program. F2perl tries to produce code that (1) works and (2) is
    clean enough that it can be modified further with a minimum amount of
    effort. Speed of the resulting code is not a consideration.

    The translated code often has a large amount of complex code at the
    start of a subroutine, but this code exists to allow the inner parts of
    a routine to remain readable and as similar to the Fortran source as
    possible.  

    An important point is that the translated code has a good chance of
    actually running correctly and thus allowing further changes to be made 
    step-by-step with validation along the way.

## BUGS AND ISSUES

    f2perl provides a good starting point for translating Fortran code to
    perl, but there are a number of open issues and problem areas that you
    need to be aware of. Here is a partial list.

  F77 only
    F2perl only translates FORTRAN 77.

  No COMPLEX
    The COMPLEX statement is not supported.

  .eqv. and .neqv. are not implemented
    You will get a warning message if these operators are found.

  I/O is incomplete
    F2perl translates simple formatted output statements, but in many cases,
    particular read operations, I/O operations are left as calls to dummy
    functions. You will have to handle these individually.

  Common Block Coding Can Be Complex
    F2perl translates common blocks and equivalence statements, but they can
    result some dense code when different names or equivalences are used for
    common block access. You'll probably want to recode them in better ways.

  Perl Code has may have special 'use' statements
    For certain IO and array operations, as well as common block access, the
    generated code may use certain modules which were installed with F2perl.
    For example, the presence of common blocks will cause f2perl to insert
    "use Fortran::F77toPerl::Common" statements to access coding in the F77toPerl
    modules. This is just to get something working. You will probably want
    to rework the code to get rid of these.

  Call by Reference Only
    At present, all call parameters are passed as references. If you know
    that certain parameters are unchanged, you may want to change them to be
    call by value.

  Data Type Conversions Won't Work
    If the Fortran source code does bit manipulations on integers, or has
    equivalences of, say, integers and floating point values, it won't work.

  Integer Division May Be Missed
    Only the simplest integer divides are correctly handled at present. So,
    if you know that your program relies on integer divides to truncate to
    integers, you'd better check the results carefully and add the
    appropriate "int" function calls.

  Speed
    The perl code will execute much, much slower than Fortran. This is
    partly due to the inherent speed of perl, but it may also be due to the
    techniques used in the translation. In particular, common block and
    multi-dimensioned array accesses involve a subroutine call. The reason
    is that this kept the resulting code readable for future modifications
    by hand. In many cases, such sections of code would benefit from a hand
    conversion to make better use of perl.

  Watch out for the LEN function
    The Fortran "LEN" function is translated into the perl "length"
    function. A warning flag is given when this happens, and you should
    check the code to see if this will work. Better yet would be to rewrite
    the code. See the test problem length.f for an example.

  Warning Messages for Common Variable Names Matching Perl Keywords
    The code which installs variables into common blocks may trigger perl
    warnings for common variables with names matching the name of a perl
    keyword. For example, the test file common.f produces common.pl, which
    when run gives these warnings:

     Ambiguous use of *{q} resolved to *q at (eval 2) line 1.
     Ambiguous use of *{x} resolved to *x at (eval 9) line 1.

    The code runs correctly. A solution would be to have f2perl capitalize such
    names to avoid this problem.
