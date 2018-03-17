#####################################################################
#
# the SourceReader class reads a fortran 77 file, one section of code at
# a time (subroutine, function, block data, program) and builds an index
# to all statement numbers.  It handles continuation lines, comments,
# tabs, and include files.
#
# The main interface routine is sub get_program_unit();
#
#####################################################################

package Fortran::F77toPerl::SourceReader;
use strict;
use warnings;

# the default will be to detab 
use Text::Tabs;
$tabstop = 8;

use IO::File;

sub new {
    my ( $class, $filename ) = @_;
    my $self = new_file($filename);
    bless $self, $class;
}

sub new_file {
    my ($filename) = @_;
    my $fh = IO::File->new("<$filename");
    unless ($fh) { die "Cannot open $filename: $!\n"; }
    return {
        _input_file        => $filename,
        _fh                => $fh,
        _input_line_number => 0,
        _stack             => [],
        _rcached_lines     => [],
        _saw_eof           => 0,
    };
}

sub putback_line {
    my $self = shift;
    if (@_) {
        push @{ $self->{_rcached_lines} }, @_;
        $self->{_input_line_number} -= @_;
    }
}

# return the next line from the input file
sub get_line {

    my $self = shift;
    my $line;
    if ( @{ $self->{_rcached_lines} } ) {
        $line = shift @{ $self->{_rcached_lines} };
    }
    else {
        if ( $self->{_saw_eof} ) {
            $line = undef;
        }
        else {
            $line = $self->{_fh}->getline();

            # detab line; this can be an option
            if ($line) {
                $line = expand($line);
            }
            else {
                $self->{_saw_eof} = 1;
            }

            ## TODO: allow free format 

            # trim to 72 columns unless it is a comment
            unless ( is_comment_or_blank($line) ) {
                if ( length($line) > 72 ) {
                    $line = substr( $line, 0, 72 );
                }
            }
        }
    }
    $self->{_input_line_number}++;

    # handle 'include' lines in any of several common formats
    if ( $line && $line =~ /^\s*#?include\s+(.+)$/i ) {
        my $new_file = $1;
        $new_file =~ s/^\s+//;
        $new_file =~ s/\s+$//;
        if    ( $new_file =~ /^\"(.+)\"$/ ) { $new_file = $1 }
        elsif ( $new_file =~ /^\'(.+)\'$/ ) { $new_file = $1 }

        my $old_self = {};
        foreach ( keys %$self ) {
            $old_self->{$_} = $self->{$_} unless $_ eq '_stack';
        }
        push @{ $self->{_stack} }, $old_self;

        my $new_self = new_file($new_file);
        foreach ( keys %$new_self ) {
            $self->{$_} = $new_self->{$_} unless $_ eq '_stack';
        }
        return $self->get_line();
    }

    else {

        # revert to previous file if at end
        if ( !defined($line) && @{ $self->{_stack} } ) {

            $self->{_fh}->close();
            my $old_self = pop @{ $self->{_stack} };
            foreach ( keys %$old_self ) {
                $self->{$_} = $old_self->{$_};
            }

            return $self->get_line();
        }

        # normal return
        else {
            return ($line);
        }
    }
}

# read one fortran statement, including possible continuation lines
sub get_statement {
    my $self = shift;
    my ($statement) = $self->get_line();
    my $continuation_line_count = 0;
    my $input_line_number       = $self->{_input_line_number};
    my $filename                = $self->{_input_file};
    if ($statement) {

        unless ( is_comment_or_blank($statement) ) {

            my @comments;
            while (1) {
                my $next_line = $self->get_line();
                unless ($next_line) {
                    $self->putback_line(@comments);
                    last;
                }
                if ( is_comment_or_blank($next_line) ) {
                    push @comments, $next_line;
                }
                elsif ( $next_line =~ /^     [^\s0]/ ) {
                    chomp $statement;
                    $next_line = substr( $next_line, 6 );
                    $statement .= $next_line;
                    $continuation_line_count++;
                }
                else {
                    $self->putback_line( @comments, $next_line );
                    last;
                }
            }
        }
    }
    return ( $statement, $input_line_number, $filename );
}

sub is_comment_or_blank {
    my ($line) = @_;
    return ( !defined($line)
          || $line =~ /^\s*$/
          || $line =~ /^\s*!/
          || $line =~ /^\s*#/
          || $line =~ /^[cC\$\*]/ );
}

sub trim {
    my ($text) = @_;
    if ($text) {
        $text =~ s/^\s*//;
        $text =~ s/\s*$//;
    }
    return $text;
}

sub get_program_unit {

    # read next section of code (subroutine, function, block data, program)
    # and build an index to all statement numbers

    my $self = shift;
    my @subroutine;
    my %label_index;
    my ( $input_line, $input_line_number, $filename, $label, $text, $saw_end );

    my $relative_line_number = '00';    # allow if ($relative_line_number) {
    while (1) {
        last if $saw_end;
        ( $input_line, $input_line_number, $filename ) = $self->get_statement();
        last unless $input_line;

        chomp $input_line;

        $text  = $input_line;
        $label = '';

        # check for comment or blank, and change comment symbol to '#'
        if ( is_comment_or_blank($text) ) {
            if ($text) { $text =~ s/^\s*\S/#/ }
            next;
        }

        # break statement into label and text and update label index;
        # FIXME: the following lines will give an error on non-fortran
        # statements; such as an isolated '}' in column 1
        # Need to avoid this.

        # patch to avoid problems with bad input
        if ( length($input_line) < 6 ) {
            print STDERR "Bad input line\n";
            $input_line = "#???:" . $input_line;
        }

        $label = trim( substr( $input_line, 0, 5 ) );
        $label =~ s/^0+//;    # leading 0's in labels are insignificant
        if ($label) {
            $label_index{$label} = $relative_line_number;
        }
        $text = trim( substr( $input_line, 6 ) );

        # check for END statement
        # allow some non-standard text after end stmt as in f90
        $saw_end =
          (      $text =~ /^\s*e\s*n\s*d$/i
              || $text =~
              /^\s*e\s*n\s*d\s+(function|subroutine|program|blockdata)/i );
    }
    continue {
        push @subroutine,
          [ $input_line, $input_line_number, $filename, $label, $text ];
        $relative_line_number++;
    }
    return @subroutine
      ? ( \@subroutine, \%label_index )
      : undef, undef;
}
1;
