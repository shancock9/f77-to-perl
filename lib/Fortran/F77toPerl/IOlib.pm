package Fortran::F77toPerl::IOlib;

use 5.006;
use strict;
use warnings;
use IO::File;
use Carp;

require Exporter;

our @ISA = qw(Exporter);

# This allows declaration	use Fortran::F77toPerl::IOlib ':all';
# If you do not need this, moving things directly into @EXPORT or @EXPORT_OK
# will save memory.
our %EXPORT_TAGS = ( 'all' => [ qw(
	
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);
our $VERSION = '0.01';

sub new {
    my ( $class ) = @_;
    my $self = bless { }, $class;
    return $self;
}

sub FH {

    # return filehandle, given unit and r/w code 
    my $self = shift;
    my ($unit,$mode)=@_; 
    unless ($self->{$unit}) {
       $self->open(unit=>$unit, mode=>$mode);
    }

    # we could check if mode=iocode here
    my ($FH, $name, $io_code) = @{$self->{$unit}};
    return $FH;
}

sub open {
    my $self=shift;
    my %defaults = (
        unit   => undef,
        file   => undef,
        mode   => undef,
        err    => undef,
        status => undef,
    );
    my %input_hash = @_;
    %input_hash = ( %defaults, %input_hash );
    my $name = $input_hash{'file'};
    my $unit = $input_hash{'unit'};
    my $mode = $input_hash{'mode'};
    my $err  = $input_hash{'err'};
    my $status  = $input_hash{'status'};
    unless ( defined($unit) ) {
        confess "attempt to open file without a fortran logical unit given\n";
    }

    ## NEED TO HANDLE STATUS HERE
    if ($status) {

    }
    unless ($name) { $name = 'fort.'.$unit;}
    unless ($mode) { $mode = 'w' }
    if ( $self->{$unit} ) {
        my ( $FH, $old_name, $old_mode ) = @{ $self->{$unit} };
        confess
"attempt to open unit $unit as $name which is already open as $old_name\n";
    }
    if ($err) {$$err=0}
    my $FH;
    if ( $name eq '-' ) {
         if ($mode eq 'r') {$FH = *STDIN;}
         elsif ($mode eq 'w') {$FH = *STDOUT;}
         else {
            confess "Unable ot open file $name in mode $mode\n"; 
         }
    }
    else {
        $FH = IO::File->new( $name, $mode );
    }
    unless ($FH) {
        if ($err) { $$err = 1 }
        else { confess "Unable ot open file $name in mode $mode\n"; }
        return $self;
    }
    @{$self->{$unit}} = ($FH,$name,$mode); 
    return $self;
}

sub close {
    my $self=shift;
    my %defaults = (
        unit   => undef,
        err    => undef,
    );
    my %input_hash = @_;
    %input_hash = ( %defaults, %input_hash );
    my $unit = $input_hash{'unit'};
    my $err  = $input_hash{'err'};
    my $status  = $input_hash{'status'};
    unless ( defined($unit) ) {
        confess "attempt to close file without a fortran logical unit given\n";
    }
    my ($FH, $name, $io_code) = @{$self->{$unit}};

    # add error check here!
    $FH->close();
    delete $self->{$unit};
    return $self;
}

1;
__END__
