#####################################################################
#
# This package has utilities for handling formats
#
#    Copyright (c) 2002-2003 by Steven L. Hancock
#    Distributed under the GPL license agreement; see file COPYING
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
#####################################################################

package Fortran::F77toPerl::Format;
use Carp;
use constant DEBUG_FORMAT_LEVEL => 0;
use strict;
BEGIN { $^W = 1; }    # turn on warnings

sub convert_to_printf {
    my ( $format ) = @_;
    my ($fmt, $count, $rscale, $errmsg)=convert_to_perl($format);
    $fmt=eval($fmt);
    return ($fmt, $count, $rscale, $errmsg);
}

sub convert_to_perl {

    # convert a fortran format string to perl code which evaluates to a
    # printf style format string
    # TODO:
    #   need to return error messages
    #   decide how to handle p, l, and :
    #   check limits of 't'
    #   require surrounding parens; error if missing
    my ( $format ) = @_;
    DEBUG_FORMAT_LEVEL && print "\nInput:$format\n";
    my $errmsg        = "";
    my $output_string = "";
    my $quote_char     = "";
    my $quoted_string  = "";
    my $msg            = "";
    my $depth          = 0;
    my @multiplicity    = (0);
    my $more_to_do     = 1;
    my $saw_dollar     = 0;
    my $io_index       = 0;
    my @scale          = (0);
    my $current_column = 0;
    my @column_width   = (0);
    my @starting_column= (0);
    my $plus           = "";   # '+' = prefix positive number with +
                               # '' =  do not prefix positive number with + 

    my $increase_column_by =sub {
        my ($cols) = @_;
        $current_column+=$cols;
        $column_width[$depth]+=$cols;
    };

    my $set_column_to =sub {
        my ($col) = @_;
        $current_column=$col;
        $column_width[$depth]=$col;
    };

    my $append_newline= sub {
        if ($output_string && $output_string !~ /\($/) {
            if ($output_string =~ /(.+)\"$/) {
               $output_string = $1 . '\n' . '"';
            }
            else {
               $output_string = $output_string . ' . ';
               $output_string .= '"\n"';
            }
        }
        else {
            $output_string .= '"\n"';
        }
        $set_column_to->(0);
    };

    my $append_and_single_quote =sub {
        my ($quote) = @_;
        $quote=~ s/\'/\\'/g;
        if ($output_string && $output_string !~ /\($/) {
            if ($output_string =~ /(.+)\'$/) {
               $output_string = $1 . $quote . "'";
            }
            else {
               $output_string .= ' . ' . "'" . $quote . "'";
            }
        }
        else {
           $output_string .= "'" . $quote . "'";
        }
    };

    my $append_repeated_quote = sub {

        # The quote should be either in quote marks or parens
        # (but not both)
        my ($quote, $count) = @_;
        if ($output_string && $output_string !~ /\($/) {
             $output_string = $output_string . ' . ';
        }
        $output_string .= "$quote x $count";
    };

    while ($more_to_do) {

        my $pos = pos($format);

        # looking for ending quote character
        if ($quote_char) {
            if ( $format =~ /\G($quote_char)/gc ) {

                # check for escaped quote
                my $pos       = pos($format);
                my $next_char = "";
                if ( $pos <= length($format) ) {
                    $next_char = substr( $format, $pos, 1 );
                }
                if ( $next_char eq $quote_char ) {
                    $quoted_string .= $1;
                    pos($format)++;
                }
                else {
                    $quote_char = "";
                    $quoted_string=~ s/\%/%%/g;
                    $increase_column_by->(length($quoted_string));
                    $append_and_single_quote->($quoted_string);
                }
            }
            elsif ( $format =~ /\G(.)/gc ) {
                $quoted_string .= $1;
            }

            # error..we reached the end without seeing the ending quote char
            else {
                    ##croak <<EOM;
print STDERR <<EOM;
Did not see ending quote character <$quote_char> in this format statement:
$format
EOM
                    last;
            }
        }

        # accumulating non-blank characters and looking for start of a 
        # quoted string
        else {

            my $pos = pos($format);
            if ( $format =~ /\G([\"\'])/gc ) {
                $quote_char    = $1;
                $quoted_string = "";
            }
            else {

                my $pos_beg = pos($format);

                # Whitespace is meaningless 
                if ( $format =~ /\G(\s+)/gc ) {
                }

                # Holerith (obsolete)
                # 7Hexample  is the same as 'example'
                elsif ( $format =~ /\G(\d[\d\s]*)[Hh]/gc ) {
                    my $chars = $1;
                    $chars =~ s/\s+//g;
                    my $pos = pos($format);

                    # handle case where something like '4H    ' may have
                    # been trimmed by the input routine
                    my $len                 = length($format);
                    my $max_chars           = $len - $pos;
                    my $missing_space_count = $chars - $max_chars;
                    if ( $missing_space_count > 0 ) {
                        $chars = $max_chars;
                    }

                    $quoted_string = substr( $format, $pos, $chars );
                    if ( $missing_space_count > 0 ) {
                        $quoted_string .= ' ' x $missing_space_count;
                    }
                    pos($format) += $chars;
                    $increase_column_by->(length($quoted_string));
                    $quoted_string=~ s/\%/%%/g;
                    $append_and_single_quote->($quoted_string);
                }

                # nX : add n blanks
                elsif ( $format =~ /\G([\d][\d\s]*)?[xX]/gc ) {
                    my $count = $1 ? $1 : 1;
                    $count=~ s/\s+//g;
                    if ($count > 1) {
                       $append_repeated_quote->("' '",$count);
                    }
                    else {
                        $append_and_single_quote->(" ");
                    }
                    $increase_column_by->($count);
                }

                # SP: + is written (Fortran 90, not 77)
                elsif ( $format =~ /\GSP/gci ) {
                   $plus = '+';
                }

                # SS: + is not written (Fortran 90, not 77)
                elsif ( $format =~ /\GSS/gci ) {
                   $plus = '';
                }

                # S: default + rule (not written) (Fortran 90)
                elsif ( $format =~ /\GS/gci ) {
                   $plus = '';
                }

                # BZ = Blanks as zeros: this is only for input  (F90)
                elsif ( $format =~ /\GBZ/gci ) {
                }

                # BN = Blanks not regarded specially; this is for input
                # (F90)
                elsif ( $format =~ /\GBN/gci ) {
                }

                # Tn - n positions from the left
                # It isn't possible to do this correctly in many cases;
                # We'll just do something that sometimes works
                elsif ( $format =~ /\G[tT]([\d][\d\s]*)/gc ) {
                    my $col = $1 ? $1 : 1;
                    $col=~s/\s+//g;
                    my $count = ($col-$current_column);
                    if ($count > 1) {
                       $append_repeated_quote->("' '",$count);
                    }

                    # just give 1 space in all other cases; cannot go back
                    else {
                        $append_and_single_quote->(" ");
                    }
                    $set_column_to->($col);
                }

                # TLn: tab left n spaces: cannot tab left; ignore
                elsif ( $format =~ /\GTL([\d][\d\s]*)/gci ) {
                }

                # TRn - tab right n positions; maybe, maybe not
                elsif ( $format =~ /\GTR([\d][\d\s]*)/gci ) {
                    my $count = $1 ? $1 : 1;
                    $count=~s/\s+//g;
                    if ($count > 1) {
                       $append_repeated_quote->("' '",$count);
                    }
                    else {
                        $append_and_single_quote->(" ");
                    }
                    $increase_column_by->($count);
                }

                # kP - Scaling factor kP
                # This is a weird one; we can't do much with it.
                # 
                # For output values, this means:
                
                # With exponent, the mantissa is multiplied by 10**k
                # and the exponent is reduced with k, which means no
                # change of the value and can avoid a leading zero.

                # Without exponent, the number is multiplied by 10**k
                # before the output, which means a change of value
                # and terribly confusing.
                elsif ( $format =~ /\G([\d][\d\s]*)?[pP]/gc ) {
                    my $count = $1 ? $1 : 1;
                    $count=~s/\s+//g;
                    $scale[$io_index]=$count;
                }

                # '/'  newline (no count allowed)
                elsif ( $format =~ /\G\//gc ) {
                    $append_newline->();
                }

                # Almost everything else.....
                elsif ( $format =~
/\G([\d][\d\s]*)?([aiefgdlzo]|en|es)\s*([\d][\d\s*]*)?(\.[\d][\d\s]*)?/gci
                  )
                {

                    my $count = $1 ? $1 : 1;
                    $count =~ s/\s+//g;
                    my $letter   = lc $2;

                    # Dw.d - same as Ew.d but for double precision
                    if ( $letter eq 'd') { $letter = 'e' }

                    # EN - engineering floating point; 3 digits before
                    # the decimal; map to E (F90)
                    if ( $letter eq 'en' ) { $letter = 'e' }

                    # ES - scientific floating point: one digit before
                    # decimal; map to E (F90)
                    if ( $letter eq 'es' ) { $letter = 'e' }

                    my $width    = $3;
                    my $decimals = $4;
                    $io_index+=$count;

                    # printf codes begin with '%'
                    my $code = '%';

                    # add the '+' code for numbers, which tells if
                    # the leading '+' is printed
                    if ( $plus && $letter =~ /^[egfi]$/ ) {
                            $code .= $plus;
                    }

                    # add the field width 
                    if ($width) {
                       $width   =~ s/\s+//g; 
                       $code .= $width
                    }
                    else {
                       $width=1;
                    }

                    # checking for valid decimal width
                    if ($decimals) {
                       $decimals   =~ s/\s+//g; 
                       if ($letter =~ /^[al]$/) {
                           print STDERR <<EOM
unexpected precision after $letter in format:
$format
EOM
                       }
                    }

                    # An - character string
                    if ( $letter eq 'a') { $code .= 's' }

                    # Z: hex (Fortran 90, not 77)
                    elsif ( $letter eq 'z') { $code .= 'x' }

                    # O: octal (Fortran 90, not 77)
                    elsif ( $letter eq 'o') { $code .= 'o' }

                    # B: binary (Fortran 90, not 77)
                    elsif ( $letter eq 'b') { $code .= 'b' }

                    # L: logical; 
                    # For now, just make it a string and require
                    # caller to convert to string.  Not much else
                    # we can do.
                    elsif ( $letter eq 'l') { $code .= 's' }

                    # Iw: Integer in field width w
                    # Iw.d means at least d digits output
                    elsif ( $letter eq 'i') {
                        if (defined($decimals)) {
                           $code .= "$decimals";
                        }
                        $code .= 'd';
                    }

                    # Ew.d, Gw.d, Fw.d  - floating point formats similar
                    # to printf.
                    elsif ( $letter =~ /^[egf]$/ ) {
                        if (defined($decimals)) {
                           $code .= "$decimals";
                        }
                        $code .= $letter;
                    }

                    else {
                        print STDERR "no handler for fmt letter '$letter'\n"; 
                        $code .= $letter;
                    }
                    if ($count>1) {
                       $append_repeated_quote->("'$code'",$count);
                    }
                    else {
                       $append_and_single_quote->("$code");
                    }
                    $increase_column_by->($width*$count);
                }

                # 'n('
                # Start a sublist which is repeated n times
                elsif ( $format =~ /\G([\d][\d\s]*)\(/gc ) {
                    $depth++; 
                    $column_width[$depth]    = 0;
                    $starting_column[$depth] = 0;
                    my $count = $1;
                    $count =~ s/\s+//g;
                    $multiplicity[$depth] = $count;
                    if ($output_string) {
                      $output_string .= ' . ';
                    }
                    $output_string .= '(';
                }

                # -----------------------------
                # single-character punctuation
                # -----------------------------
                elsif ( $format =~ /\G(.)/gc ) {
                    my $tok = $1;

                    # '(' starts a sublist
                    if ( $tok eq '(' ) {
                        $depth++;
                        $column_width[$depth]    = 0;
                        $starting_column[$depth] = 0;
                        $multiplicity[$depth] = 0;
                    }

                    # ')' ends a sublist
                    elsif ( $tok eq ')' ) {
                        if ( $multiplicity[$depth] ) {
                            $output_string .= ") x $multiplicity[$depth] ";
                            $set_column_to->($starting_column[$depth]+ 
                               $multiplicity[$depth]*$column_width[$depth]); 
                        }
                        $depth--;
                    }
                    elsif ( $tok eq ',' ) {

                    }

                    # ':' discontinue -- means stop here if end of list;
                    # that's hard to implement with printf
                    elsif ( $tok eq ':' ) {

                    }

                    # '$' means no terminal <cr>
                    # Non-standard but common
                    elsif ( $tok eq '$' ) {
                        $saw_dollar=1;
                    }

                    else {
                        print STDERR <<EOM;
unexpected character '$tok' in format statement:
$format
EOM
                    }
                }

                # that's all..
                else {
                    last;
                }
            }
        }
    }
    $append_newline->() unless $saw_dollar;

    if ( $depth != 0 ) {
        print STDERR "unbalanced parens, final depth=$depth in : $format\n";
    }
    DEBUG_FORMAT_LEVEL && do {
        print "Output:($output_string)\n";
        print "Items:($io_index)\n";
    };
    return ( $output_string, $io_index , \@scale, $errmsg);
}

1;
