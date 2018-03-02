#####################################################################
#
# a class to manage one-time dealing out items from a list
#
#####################################################################

package Fortran::F77toPerl::Lister;
use strict;
BEGIN { $^W = 1; }    # turn on warnings

sub new {
    my ( $class, $rlist ) = @_;
    my $self = bless $rlist, $class;
    return $self;
}
sub count { my $self = shift; return @$self }
sub get_next  { my $self = shift; return shift @$self }
sub get_list {
    my $self    = shift;
    my $num     = shift;
    my @sublist = splice @$self, 0, $num;
    return wantarray ? @sublist : \@sublist;
}

sub get_list_or_constant {

    # See if the next $num items are the same, and return that
    # value if so. Otherwise return the list.
    my $self = shift;
    my $num  = shift;
    my @sublist = splice @$self, 0, $num;
    my $val  = $sublist[0];
    foreach my $i ( 1 .. $num - 1 ) {
        if ( $sublist[$i] ne $val ) {
            return wantarray ? @sublist : \@sublist;
        }
    }
    return $val;
}
1;
