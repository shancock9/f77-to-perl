package Fortran::F77toPerl::Common;
use strict;
BEGIN { $^W = 1; }    # turn on warnings
use vars qw(
   $block_maker
);

# Define code, as a string, needed to create the common variables for
# each common block.  This is nasty stuff, and relies on the
# questionable assumption that perl wont move the COMMON array
# (which has a fixed size).  TODO: pin down the array size limit 
# in the beginning.

BEGIN {

$block_maker = <<'EOM';

        my $offset = 0;
        ##our %offset;
        my %array_size;
        my %index_string;
        my $position_array; 
        my $position_scalar; 

        $position_scalar = sub {

            # code to place a scalar with name $name at offset $offset
            # from the beginning of common
            my ( $name, $offset ) = @_;
            no strict 'refs';
            $Offset{$name} = $offset;
            *{$name} = \$COMMON->[ $offset ];
            ## my $pkg=__PACKAGE__;
            ## print STDERR "position scalar $name at $offset in $pkg \n";
        };

        $push_scalars = sub {
            foreach (@_) {
                $position_scalar->($_, $offset);
                $offset++;
            }
        };

        $declare_array = sub {

            # compute and store array size and index string
            my $name = shift;
            my $n    = @_ / 2 - 1;
            my $imax = pop @_;
            my $imin = pop @_;
            my $d    = $imax - $imin + 1;
            if ( $d < 0 ) { die "array $name has imin=$imin imax=$imax\n" }
            my $size         = $d;
            my $str = "\$_[$n]";
            if ($imin < 0) {
                 $imin = eval(-$imin);
                 $str.= " + $imin";
            }
            elsif ($imin > 0) {
               $str .= " - $imin";
            }
            my $index_string = $str;
            for ( my $i = $n - 1 ; $i >= 0 ; $i-- ) {
                $imax = pop @_;
                $imin = pop @_;
                $d    = $imax - $imin + 1;
                if ( $d < 0 ) { die "array $name has imin=$imin imax=$imax\n" }
                my $str = "\$_[$i]";
                if ($imin < 0) {
                     $imin = eval(-$imin);
                     $str.= " + $imin";
                }
                elsif ($imin > 0) {
                   $str .= " - $imin";
                }
                $index_string = $str . " + $d * ( $index_string )";
                $size *= $d;
            }
            $index_string{$name} = $index_string;
            $array_size{$name}   = $size;
        };

        $declare_array_min1=sub {

            # simplified call for case where imin =1 for all dimensions
            my @newargs=(shift);
            while(@_) {
                 push @newargs,1;
                 push @newargs, shift;
            }
            $declare_array->(@newargs);
        };

        $position_array = sub {

            # code to place an array with name $name at offset $offset
            # from the beginning of common
            my ($name, $offset) = @_;
            $Offset{$name} = $offset;
            my $index_string = $index_string{$name};
            my $code         =
              "\*{$name}=\\sub : lvalue { \$COMMON->[ $offset + $index_string]};";
            ##print STDERR "evaling code $code for $name at offset $offset\n";
            eval "$code";
            if ($@) { die "Error evaling the following code: $@\n$code\n" }

            # code to create $C_array, which is needed for passing an
            # offset array as an argument
            $code =
"\*{C_$name}=\\sub {[\$COMMON, (\@_ > 0) ? $offset + $index_string : $offset]};";
##print STDERR "code is $code\n";
              ##print STDERR "evaling code $code for $name at offset $offset\n";
            eval "$code";
            ##print STDERR "done evaling code $code for $name at offset $offset\n";
            if ($@) { die "Error evaling the following code: $@\n$code\n" }
        };

        $push_array = sub {

            # code to declare and place an array in common,
            # such that $array->($i,$j) gives the i,j th array element
            my $name = $_[0];
            $declare_array->(@_);
            $position_array->($name,$offset);
            my $size = $array_size{$name};
            $offset += $size;
            ##print STDERR "is $offset after $name\n";
        };

        $push_array_min1=sub {

            # simplified call for case where imin =1 for all dimensions
            my @newargs=(shift);
            while(@_) {
                 push @newargs,1;
                 push @newargs, shift;
            }
            $push_array->(@newargs);
        };

        $equivalence = sub {

            # old is any variable already positioned in common
            # new is a new array or scalar
            # if it is an array, $declare_array-> must have been called
            my ( $new, $old ) = @_;
            my $new_name    = shift @$new;
            my $old_name    = shift @$old;
            my $old_defined = defined( $Offset{$old_name} );
            my $new_defined = defined( $Offset{$new_name} );
            if ( $new_defined && !$old_defined ) {
                ( $old_name, $new_name ) = ( $new_name, $old_name );
                ( $old,      $new )      = ( $new,      $old );
            }
            elsif ( $old_defined && $new_defined ) {
                die
"Attempting to equivalance $old_name and $new_name, which are already in common\n";
            }
            elsif ( !$old_defined && !$new_defined ) {
                die
"Attempting to equivalance $old_name and $new_name, but neither is in common\n";
            }
            my @new_dims = @$new;
            my @old_dims = @$old;
            my $get_index;
            my $is_new_array = defined( $index_string{$new_name} );
            ##print STDERR "new name is $new_name\n";
            ##print STDERR "old name is $old_name\n";
            my $old_index = 0;    ##$Offset{$old_name};

            if (@$old) {
                if ( !defined( $index_string{$old_name} ) ) {
                    die
"attempting to equivalance to scalar $old_name with dimensioins @$old\n";
                }
                my $code = "\$get_index = sub {$index_string{$old_name}};";
                eval($code);
                if ($@) {
                    die
"error eval'ing equivalance for new=$new_name and old=$old_name\n";
                }
                $old_index += $get_index->(@$old);
                ##print STDERRR "code is $code\n";
            }
            ##print STDERR "$old_name index is: $old_index\n";
            ##print STDERR "$old_name offset is: $Offset{$old_name}\n";
            ##print STDERR "\$new is $new\n";
            my $new_index = 0;    ##$Offset{$new_name};
            if (@$new) {
                if ( !defined( $index_string{$new_name} ) ) {
                    die
"attempting to equivalance undeclared array $new_name with dimensioins @$old\n";
                }
                my $code = "\$get_index = sub {$index_string{$new_name}};";
                eval($code);
                if ($@) {
                    die
"error eval'ing equivalance for new=$new_name new=$new_name\n";
                }
                $new_index += $get_index->(@$new);
                ##print STDERR "is $code\n";
            }
            ##print STDERR "new index is: $new_index\n";

            my $new_offset = $Offset{$old_name} + $old_index - $new_index;
            if ( $new_offset < 0 ) {
                die
                  "Negative offset when equivalancing $new_name to $old_name\n";
            }
            if ($is_new_array) {
                $position_array->( $new_name, $new_offset );
            }
            else {
                $position_scalar->( $new_name, $new_offset );
                ##print STDERR "EQUIVALENCE $new_name at $new_offset\n";
            }
        };
EOM
}

1;
