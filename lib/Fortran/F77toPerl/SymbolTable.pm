#####################################################################
#
# The Fortran::F77toPerl::SymbolTable Class handles storing and extracting
# all symbols.  Basically, just a hash with some access control.
#
#####################################################################

package Fortran::F77toPerl::SymbolTable;
use strict;
use warnings;
use Carp;

sub new {

    my ( $class, $filename ) = @_;
    bless {}, $class;
}

# The set routine allows multiple properties to be set in one call
sub set {
    my $self = shift;

    # list of allowed property names
    my %defaults = (
        name                   => undef,
        storage_type           => undef,  # one of these:
                                          # '$' scalar
                                          # '@' array
                                          # 'x' code (function)
                                          # '$x' statement function
                                          # 'vx' pure call by value function
        fortran_data_type      => "",     # one of these:
                                          # character
                                          # complex
                                          # doubleprecision
                                          # integer
                                          # logical
                                          # real
        is_arg                 => undef,  # true if came in arg list
        need_access_sub        => undef,  # uses $a->(...) sub
        need_call_sub          => undef,  # uses $C_a->(...) sub
        save                   => undef,  # true if saved
        imins                  => undef,  # ref to array with min indexes
        imaxs                  => undef,  # ref to array with max indexes
        imin_values            => undef,  # ref to array with evaluated
                                          # min indexes
        imax_values            => undef,  # ref to array with evaluated
                                          # max indexes
        array_size             => undef,  # total number of storage locations
                                          # used by this array
        dependencies           => undef,  # ref to array of parameter names
        index_string           => undef,  # string for sub array access
        is_common              => undef,  # true if is in common block
        common_name            => undef,  # name of ref to array like "COMMON_a"
        common_offset          => undef,  # offset for array access
        common_package_name    => undef,  # name of package with this
                                          # common variable, if any.
        marked                 => undef,  # utility flag
        equivalence_to         => undef,
        equivalence_lindex     => undef,
        equivalence_rindex     => undef,
        local_equivalence_name => undef,
        local_equivalence_offset => undef,
    );

    my %input_hash = @_;
    if (
        my @bad_keys =
        grep { !exists $defaults{$_} } keys %input_hash
      )
    {
        local $" = ')(';
        my @good_keys = sort keys %defaults;
        @bad_keys = sort @bad_keys;
        confess <<EOM;
------------------------------------------------------------------------
Unknown symbol table parameter : (@bad_keys)
Symbol Table only understands  : (@good_keys)
------------------------------------------------------------------------

EOM
    }

    my $name = $input_hash{'name'};
    unless ($name) {
        confess <<EOM;
Symbol Table requires a symbol name
EOM
    }

    # Set defaults for new symbol
    unless ( defined( $self->{$name} ) ) {
        %input_hash = ( %defaults, %input_hash );
    }

    foreach my $key ( keys(%input_hash) ) {
        if ( $key ne 'name' ) {
            my $val = $input_hash{$key};
            $self->{$name}->{$key} = $input_hash{$key};
##            if ($key eq 'imaxs') {
##                    if ($input_hash{$key}) {
##                    my $list=join ",", @{$input_hash{$key}};
##                    print "DEBUG: storing for $name $list\n";
##                    }
##                    else {
##                      print "DEBUG: storing undef for $name \n";
##                    }
##            }
        }
    }
}

sub get_symbols {

    my $self = shift;
    return wantarray ? sort keys %$self : \( sort keys %$self );
}

sub dump {

    # temporary debug routine; use data dumper
    my $self = shift;
    print "---Local Symbol Table---\n";
    foreach my $name ( sort keys %$self ) {
        print "$name\n";
    }
}

sub defined {
    my $self = shift;
    my ($name) = @_;
    return defined( $self->{$name} );
}

# The get routine allows one property to be retrieved per call
# returns undef if name is not defined
sub get {
    my $self = shift;
    my ( $name, $property ) = @_;

    # convert to bareword if necessary
    $name =~ s/^\$+//;
    if ( defined( $self->{$name} ) ) {

##            if ($property eq 'imaxs') {
##                    if ($self->{$name}->{$property}) {
##                    my $list=join ",", @{$self->{$name}->{$property}};
##                    print "DEBUG: fetching for $name $list\n";
##                    }
##                    else {
##                      print "DEBUG: fetching undef for $name \n";
##                    }
##            }
##
        return $self->{$name}->{$property};
    }
    else {
        return;
    }
}

sub copy {
    my $self = shift;
    my ( $oldname, $newname ) = @_;
    if ( defined( $self->{$oldname} ) ) {
        $self->{$newname} = $self->{$oldname};
    }
}

sub delete {
    my $self = shift;
    my ($name) = @_;
    if ( exists( $self->{$name} ) ) { delete $self->{$name} }
}

1;
