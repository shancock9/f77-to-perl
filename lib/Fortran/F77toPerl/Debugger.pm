#####################################################################
#
# The Fortran::ToPerl::Debugger class shows line tokenization
#
#####################################################################

package Fortran::ToPerl::Debugger;
use strict;
BEGIN { $^W = 1; }    # turn on warnings

sub new {

    my ( $class, $filename ) = @_;

    bless {
        _debug_file        => $filename,
        _debug_file_opened => 0,
        _fh                => undef,
    }, $class;
}

sub really_open_debug_file {

    my $self       = shift;
    my $debug_file = $self->{_debug_file};
    my $fh;
    unless ( $fh = IO::File->new("> $debug_file") ) {
        warn("can't open $debug_file: $!\n");
    }
    $self->{_debug_file_opened} = 1;
    $self->{_fh}                = $fh;
    print $fh <<EOM;
For each non-comment input line of the file there are 2 lines.
The first line shows line number and squeezed fortran text
The second line shows the line number and the token types
EOM
}

sub close_debug_file {

    my $self = shift;
    my $fh   = $self->{_fh};
    if ( $self->{_debug_file_opened} ) {

        eval { $self->{_fh}->close() };
    }
}

sub write_debug_entry {

    # This is a debug dump routine which may be modified as necessary
    # to dump tokens on a line-by-line basis.  The output will be written
    # to the .DEBUG file when the -D flag is entered.
    my $self           = shift;
    my $line_of_tokens = shift;

    my $input_line        = $line_of_tokens->{_line_text};
    my $input_line_number = $line_of_tokens->{_line_number};
    my $rtokens           = $line_of_tokens->{_rtokens};
    my $rtoken_type       = $line_of_tokens->{_rtoken_types};
    my $line_type         = $line_of_tokens->{_line_type};

    my ( $j, $num );

    my $token_str              = "$input_line_number: ";
    my $reconstructed_original = "$input_line_number: ";
    my $block_str              = "$input_line_number: ";

    my @next_char = ( '"', '"' );
    my $i_next = 0;
    unless ( $self->{_debug_file_opened} ) {
        $self->really_open_debug_file();
    }
    my $fh = $self->{_fh};

    for ( $j = 0 ; $j < @$rtoken_type ; $j++ ) {

        $reconstructed_original .= $$rtokens[$j];
        $num = length( $$rtokens[$j] );
        my $type_str = $$rtoken_type[$j];

        # be sure there are no blank tokens (shouldn't happen)
        # This can only happen if a programming error has been made
        # because all valid tokens are non-blank
        if ( $type_str eq ' ' ) {
            print $fh "BLANK TOKEN on the next line\n";
            $type_str = $next_char[$i_next];
            $i_next   = 1 - $i_next;
        }

        if ( length($type_str) == 1 ) {
            $type_str = $type_str x $num;
        }
        $token_str .= $type_str;
    }

    print $fh "$reconstructed_original\n";
    print $fh "$token_str\n";

}

1;
