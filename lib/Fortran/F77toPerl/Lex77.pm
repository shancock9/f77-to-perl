#####################################################################
#
# This package takes a line of text and breaks it into tokens
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

package Fortran::F77toPerl::Lex77;
use strict;
BEGIN { $^W = 1; }    # turn on warnings

{
    my @statement_types;
    my %is_keyword;

    BEGIN {

        # DO NOT SORT THIS LIST
        # because statement types are tested in sequential order.
        # for example, 'end' must come after 'endif' and 'enddo'
        # and 'do' must come after 'doubleprecision'
        #
        # NOTE:
        # dowhile is non-standard and must come before 'do'
        @statement_types = qw(
          assign
          backspace
          blockdata
          call
          character
          close
          common
          complexdouble
          complex
          continue
          data
          dimension
          doubleprecision
          doublecomplex
          dowhile
          do
          elseif
          else
          endfile
          endif
          enddo
          end
          entry
          external
          equivalence
          format
          function
          goto
          if
          implicit
          inquire
          integer
          intrinsic
          logical
          open
          parameter
          pause
          print
          program
          read
          real
          return
          rewind
          save
          stop
          subroutine
          write
        );

        # non-standard for f77
        # Could be turned off as an option
        # see also 'dowhile' above
        @_ = qw{
          cycle
          exit
          encode
          decode
          namelist
          accept
          type
          byte
        };
        push @statement_types, @_;
    }

    sub error {
        print STDERR "Error: " . $_[0];
    }

    sub warning {
        print STDERR "Warning: " . $_[0];
    }

    my %is_valid_token;
    my %is_dot_token;
    my %is_data_type;

    BEGIN {
        @_ = qw{! $ ( ) = + - / * . : };
        push @_, '\\';
        push @_, ',';
        @is_valid_token{@_} = (1) x scalar(@_);

        @_ =
          qw{ .eq. .ne. .le. .lt. .gt. .ge. .not. .and. .or. .eqv. .neqv. .true. .false.};
        @is_dot_token{@_} = (1) x scalar(@_);

        @_ = qw{
          character
          complexdouble
          doubleprecision
          doublecomplex
          complex
          integer
          logical
          real
        };
        @is_data_type{@_} = (1) x scalar(@_);
    }

    sub tokenize_fortran_statement {

        # Given the text of a fortran statement (without label),
        # split it into tokens and identify the type of statement
        #
        # Input:
        # -----
        #  $input_stmt    = complete text of a fortran statement, without
        #                   label but with all continuation lines included.
        #  $input_line_number = the number of this line in the input file
        #
        # Output:
        # ------
        #  $rline_of_tokens = reference to hash of tokenization information
        #                   see end of soubroutine for contents
        #  $rmore_text    = if defined, this is a reference to a stack of
        #                   additional lines of text to be processed in order
        #                   to complete this statement. This will happen when
        #                   a one-line if is converted to a multi-line 
        #                   if-then-endif, and also when a mid-line ';'
        #                   is encountered.  This stack should be pushed
        #                   onto an outer stack of text, which should be
        #                   popped to get the next text before another
        #                   input statement is processed.

        # Token types:
        # k - fortran 'keyword' of some type (if, then, else ...)
        # w - any unidentified bareword
        # d - number of any type except hex
        # z - hex number
        # Q - quote
        # # - comment
        # .eq. .le. etc
        # The following have their fortran meanings:
        # ( ) / * + - ** //

        # "Consistently separating words by spaces became a general 
        # custom about the tenth century A.D., and lasted until
        # about 1957, when FORTRAN abandoned the practice."    
        #    - Sun FORTRAN Reference Manual .

        my ( $input_stmt, $input_line_number ) = @_;

        my @tokens    = ();
        my @token_map = (0);    # string position of start of each token
        my @types     = ();
        my @more_text = ();

        my $lex_error     = 0;
        my $quote_char    = "";
        my $quoted_string = "";
        my $msg           = "";
        my $stmt_type     = "";
        my $side_comment  = "";
        my $type;

        my $depth = 0;
        my ( $comma_count, $equals_count );
        my $leading_if = ( $input_stmt =~ /^\s*i\s*f\s*\(/i );
        my $more_to_do = 1;
        my $tok;
        my $last_nonblank_pre_type = 'b';

        # Main tokenization loop --
        # Tokenize a statement in one pass 
        # and fix things up at the end.
        while ($more_to_do) {

            my $pos = pos($input_stmt);

            # ------------------------------------------
            # Mode 2: looking for ending quote character
            # ------------------------------------------
            if ($quote_char) {
                if ( $input_stmt =~ /\G($quote_char)/gc ) {

                    # check for escaped quote
                    my $pos       = pos($input_stmt);
                    my $next_char = "";
                    if ( $pos <= length($input_stmt) ) {
                        $next_char = substr( $input_stmt, $pos, 1 );
                    }
                    if ( $next_char eq $quote_char ) {
                        $quoted_string .= $1;
                        pos($input_stmt)++;
                    }
                    else {

                        # check for hex
                        # NOTE: the letter must immediately follow the quote,
                        # no spaces allowed
                        if (   $next_char =~ /[xz]/i
                            && $quoted_string
                            && $quoted_string =~ /^[0-9a-f\s]+$/i
                            && $quoted_string !~ /^\s*$/)
                        {
                            push @types,  'Z';
                            push @tokens, "Z'$quoted_string'";
                            pos($input_stmt)++;
                        }

                        # check for octal
                        elsif ($next_char =~ /[o]/i
                            && $quoted_string =~ /^[0-7\s]+$/ 
                            && $quoted_string !~ /^\s*$/)
                        {
                            push @types,  'O';
                            push @tokens, "O'$quoted_string'";
                            pos($input_stmt)++;
                        }

                        # check for binary
                        elsif ($next_char =~ /[B]/i
                            && $quoted_string =~ /^[0-1\s]+$/ 
                            && $quoted_string !~ /^\s*$/)
                        {
                            push @types,  'B';
                            push @tokens, "B'$quoted_string'";
                            pos($input_stmt)++;
                        }

                        # must be a quote
                        else {
                            push @types,  'Q';
                            push @tokens, $quoted_string;
                        }
                        $quote_char = "";
                    }
                }
                elsif ( $input_stmt =~ /\G(.)/gc ) {
                    $quoted_string .= $1;
                    ##print "DEBUG Continuing quote; string=$squeezed_stmt\n"; 
                }

                # error..we reached the end without seeing the ending quote char
                else {
                    $lex_error = 1;
                    $msg       = <<EOM;
Did not see ending quote character <$quote_char> in this text:
$input_stmt
EOM
                    error($msg);
                    last;
                }
            }

            # ------------------------------------------
            # Mode 1: accumulating non-blank characters 
            # and looking for start of a quoted string
            # ------------------------------------------
            else {

                my $pos = pos($input_stmt);

                # start of a quoted string
                # Note: A common Fortran extension is to allow a " character
                if ( $input_stmt =~ /\G([\"\'])/gc ) {
                    $quote_char    = $1;
                    $quoted_string = "";
                }

                else {

                    my $pre_type;
                    my $pos_beg = pos($input_stmt);

                    # whitespace
                    if ( $input_stmt =~ /\G(\s+)/gc ) {
                        $pre_type = 'b';
                        $tok      = $1;
                    }

                    # holerith
                    elsif (

                        # Holerith cannot follow '*' which is the 2nd token
                        # For example, this is not holerith:
                        # character*8 hed(20,30). 
                        !( $last_nonblank_pre_type eq '*' && @tokens == 2 )

                        # Note that this is not holerith:
                        # do 1000 hh=1,10
                        # Holerith can follow ( , = + - / * //
                        # Because of the '//', we'll just anchor to the end
                        # in this regex.
                        && $last_nonblank_pre_type =~ /[\(\,\=\+\-\/\*]$/

                        # Also, note that the regex test must come last here,
                        # otherwise characters will be lost. 
                        && $input_stmt =~ /\G(\d[\d\s]*)[Hh]/gc
                      )
                    {
                        my $chars = $1;
                        $chars =~ s/\s*//g;
                        my $pos = pos($input_stmt);

                        # handle case where something like '4H    ' may have
                        # been trimmed by the input routine
                        my $len                 = length($input_stmt);
                        my $max_chars           = $len - $pos;
                        my $missing_space_count = $chars - $max_chars;
                        if ( $missing_space_count > 0 ) {
                            $chars = $max_chars;
                        }

                        $quoted_string = substr( $input_stmt, $pos, $chars );
                        if ( $missing_space_count > 0 ) {
                            $quoted_string .= ' ' x $missing_space_count;
                        }
                        pos($input_stmt) += $chars;
                        $pre_type = 'Q';
                        $tok      = $quoted_string;
                    }

                    # ----------------------------------
                    # numbers with digits after decimals
                    # ----------------------------------
                    elsif ( $input_stmt =~
/\G([\d\s]*(\.[\d][\d\s]*)([EeDdQq]\s*[+-]?(\s*\d[\d\s]*))?)/gc
                      )
                    {
                        $pre_type = 'd';
                        $tok      = $1;
                    }

                    # ----------------------------------
                    # numbers with leading digits
                    # ----------------------------------
                    elsif ( $input_stmt =~
/\G([\d][\d\s]*(\.[\d\s]*)?([EeDdQq]\s*[+-]?(\s*\d[\d\s]*))?)/gc
                      )
                    {
                        $pre_type = 'd';
                        $tok      = $1;

                        # Watch out for something like 3.eq.
                        if ( $tok =~ /(\.\s*)$/ ) {
                            $pos = pos($input_stmt);
                            my $chars = 0;
                            if ( $input_stmt =~ /\G(\s*[a-z][a-z\s]*\.)/gci ) {
                                $tok =~ s/(\.\s*)$//;
                                $chars = length($1);
                            }
                            pos($input_stmt) = $pos - $chars;
                        }
                    }

                    # -----------------------------
                    # hex numbers
                    # -----------------------------
                    elsif ( $input_stmt =~ /\G([xz]\s*'[\s0-9a-f]+\s*')/gci ) {
                        $tok      = lc $1;
                        $pre_type = 'Z';
                    }
                    
                    # -----------------------------
                    # octal numbers
                    # -----------------------------
                    elsif ( $input_stmt =~ /\G([o]\s*'[\s0-7]+\s*')/gci ) {
                        $tok      = lc $1;
                        $pre_type = 'O';
                    }

                    # -----------------------------
                    # binary numbers
                    # -----------------------------
                    elsif ( $input_stmt =~ /\G([b]\s*'[\s0-1]+\s*')/gci ) {
                        $tok      = lc $1;
                        $pre_type = 'B';
                    }

                    # ----------------------------------
                    # any words; allow VAX interior '$'s
                    # fix them later
                    # ----------------------------------
                    elsif ( $input_stmt =~ /\G([\w][\w\$]*)/gc ) {
                        $pre_type = 'w';
                        $tok      = lc $1;
                    }

                    # -----------------------------
                    # logical operators like .eq. .and. etc
                    # -----------------------------
                    elsif ( $input_stmt =~ /\G(\.\s*[a-z][a-z\s]*\.)/gci ) {
                        $pre_type = lc $1;
                        $pre_type =~ s/\s*//g;
                        $tok = $pre_type;
                        unless ( $is_dot_token{$tok} ) {
                            print STDERR "Syntax error at '$tok'\n";
                            $lex_error = 1;
                        }
                    }

                    # -----------------------------
                    # double-character punctuation
                    # -----------------------------

                    # // concatenation
                    # NOTE: There is an ambiguity in format statements,
                    # where '/' has special meaning and may be repeated.
                    # But we don't yet know if this is a format statement.
                    # So we will assume concatenation and fix this later
                    # if necessary.
                    elsif ( $input_stmt =~ /\G(\/\s*\/)/gci ) {
                        $pre_type = $tok = '//';
                    }

                    # **
                    elsif ( $input_stmt =~ /\G(\*\s*\*)/gci ) {
                        $pre_type = $tok = '**';
                    }

                    # %val, %loc, %ref are common extensions
                    elsif ( $input_stmt =~ /\G(\%val)/gci ) {
                        $pre_type = 'k';
                        $tok      = lc $1;
                    }
                    elsif ( $input_stmt =~ /\G(\%loc)/gci ) {
                        $pre_type = 'k';
                        $tok      = lc $1;
                    }
                    elsif ( $input_stmt =~ /\G(\%ref)/gci ) {
                        $pre_type = 'k';
                        $tok      = lc $1;
                    }

                    # -----------------------------
                    # single-character punctuation
                    # -----------------------------
                    elsif ( $input_stmt =~ /\G(\W)/gc ) {
                        $tok      = $1;
                        $pre_type = $1;
                        if    ( $tok eq '(' ) { $depth++ }
                        elsif ( $tok eq ')' ) {
                            $depth--;

                            # check for the rest of an if statement
                            if ( $leading_if && $depth eq 0 ) {
                                my $pos       = pos($input_stmt);
                                my $remainder = substr( $input_stmt, $pos );
                                if (   $remainder
                                    && $remainder =~ /^\s*[A-Za-z_]/ )
                                {
                                    $stmt_type = 'if';

                                    # convert a one-line if of the form: 'if
                                    # (xxx) yyy' into a multi-line
                                    # if-then-endif stmt
                                    unless ( $remainder =~
                                        /^\s*(\d|t\s*h\s*e\s*n\s*)(!.*)?$/i )
                                    {
                                        $more_to_do = 0;
                                        if ($2) {$side_comment=$2};
                                        push @types,     $pre_type;
                                        push @tokens,    $tok;
                                        push @token_map, pos($input_stmt);

                                        push @types,     'k';
                                        push @tokens,    'then';
                                        push @token_map, pos($input_stmt);

                                        push @more_text, 'endif';
                                        push @more_text, $remainder;
                                        last;
                                    }
                                }
                            }
                        }
                        elsif ( $tok eq ',' && $depth == 0 ) {
                            $comma_count++;
                        }
                        elsif ( $tok eq '=' && $depth == 0 ) {
                            $equals_count++;
                        }

                        # allow ';' multi-statement terminator
                        elsif ( $tok eq ';' ) {
                            my $pos = pos($input_stmt);
                            my $remainder = substr( $input_stmt, $pos );
                            if ($remainder) {
                                push @more_text, $remainder;
                            }
                            else {

                                # This should be a warning
                                print STDERR "Useless terminal ';'\n";
                            }
                            last;
                        }

                        # look for side comments beginning with '!'
                        elsif ( $tok eq '!' ) {
                            $side_comment =
                              '#' . substr( $input_stmt, pos($input_stmt) );
                            last;
                        }
                        elsif ( !$is_valid_token{$tok} ) {
                            error("Invalid F77 token:($tok)\n");
                            my @valid = keys %is_valid_token;
                            print STDERR "valid tokens=(@valid)\n";
                            $lex_error = 1;
                        }
                    }

                    # that's all..
                    else {
                        last;
                    }

                    # store non-blank tokens
                    unless ( $pre_type eq 'b' ) {

                        if ( $pre_type ne 'Q' ) {
                            $tok =~ s/\s+//g;
                        }

                        ##print "DEBUG Storing token $tok type $pre_type\n";

                        # concatenate any words separated by blanks
                        if (   $pre_type =~ /^[wd]$/
                            && $tok =~ /^\w+$/
                            && @types
                            && $types[$#types] eq 'w' )
                        {
                            $tokens[$#tokens] .= $tok;
                        }

                        # otherwise make new token entry
                        else {
                            push @types,     $pre_type;
                            push @tokens,    $tok;
                            push @token_map, pos($input_stmt);
                        }
                        $last_nonblank_pre_type = $pre_type;
                    }
                }
            }
        }

        if ( $depth != 0 ) {
            print STDERR "Unbalanced parens: $input_stmt\n";
            $lex_error = 1;
        }

        goto FINISHED if $lex_error;

        # identify the statement type if we haven't done so already
        # (any 'if' statements have already been identified)
        if ($stmt_type) {
            $types[0] = 'k';
            if (   $tokens[0] eq 'if'
                && $tokens[$#tokens] eq 'then' )
            {
                $types[$#tokens] = 'k';
            }
        }
        else {

            # It should not begin with an identifier if it has any commas
            # outside of parens, or if it does not have an equals outside
            # of parens
            if ( $comma_count || !$equals_count ) {
                if ( $types[0] eq 'w' ) {
                    my $leading_text = $tokens[0];

                    foreach (@statement_types) {
                        if ( $leading_text =~ /^($_)(.*)$/ ) {
                            $stmt_type = $_;
                            if ($2) {
                                $tokens[0] = $2;

                                # Now we have to pay a price for being greedy
                                # in combining letters and digits.
                                # If the remaining token has leading digits,
                                # we have to split them out as a number.
                                # For example, something like:
                                # 'do100m' should become 'do' '100' 'm'
                                # with types 'k' 'd' 'w'
                                if ( $tokens[0] =~ /^(\d+)(.*)$/ ) {
                                    if ($2) {
                                        $tokens[0] = $2;
                                        $types[0]  = 'w';

                                        unshift @tokens,    $1;
                                        unshift @types,     'd';
                                        unshift @token_map, 0;
                                    }
                                    else {
                                        $types[0] = 'd';
                                    }
                                }
                                unshift @tokens,    $stmt_type;
                                unshift @types,     'k';
                                unshift @token_map, 0;
                            }
                            else {
                                $types[0] = 'k';
                            }

                            # fixup specific statements
                            if ( $stmt_type eq 'elseif' ) {
                                if ( $tokens[$#tokens] eq 'then' ) {
                                    $types[$#tokens] = 'k';
                                }
                                else {
                                    print STDERR
"Missing 'then' after 'elseif':$input_stmt\n";
                                    $lex_error = 1;
                                }
                            }

                            # we have to split the 'to' from the variable in
                            # assign ddd to nvar
                            elsif ( $stmt_type eq 'assign' ) {
                                my $last_token = $tokens[$#tokens];
                                if (   $#tokens eq '2'
                                    && $last_token =~ /^to(\w+)$/ )
                                {
                                    $tokens[2] = 'to';
                                    $types[2]  = 'k';
                                    $tokens[3] = $1;
                                    $types[3]  = 'w';
                                }
                                else {
                                    print STDERR
                                      "Syntax error in 'assign' statement\n";
                                    $lex_error = 1;
                                }
                            }

                            # look for function and name after data type, like:
                            #    character*(*) function xx
                            elsif ( $is_data_type{$stmt_type} ) {

                                for ( my $i = 1 ; $i <= $#tokens ; $i++ ) {
                                    if ( $tokens[$i] =~ /^function(\w+)$/ ) {
                                        $stmt_type = 'function';
                                        $tokens[$i] = 'function';
                                        $types[$i]  = 'k';
                                        splice @tokens, $i + 1, 0, $1;
                                        splice @types,  $i + 1, 0, 'w';
                                        splice @token_map, $i + 1, 0,
                                          $token_map[$i];
                                        my $tok_line  = join "", @tokens;
                                        my $type_line = join "", @types;
                                        last;
                                    }

                                    # skip past stuff like *(*), *70, etc
                                    last
                                      unless ( $tokens[$i] =~ /^[\d\*\(\)]/ );
                                }
                            }
                            last;
                        }
                    }
                    unless ($stmt_type) {
                        error("unknown statement type: $input_stmt\n");
                        $lex_error = 1;
                    }
                }
                else {
                    error("unknown statement type: $input_stmt\n");
                    $lex_error = 1;
                }
            }
        }

        # NOTE: here we could split any concatenation operators into
        # two slashes.  For now, the caller does it.
        # if ($stmt_type eq 'format') {
        # }

        #  Create the hash of return values.  These include:
        #  $stmt_type     = type of statement (usually leading fortran word) or
        #                   just "" if regular statement
        #  $lex_error     = fatal error of some kind; statement not valid
        #  \@tokens       = ref to array of tokens
        #  \@types        = ref to array of token types
        #  \@token_map    = ref to array mapping tokens back to input line
        #                   (for error messages)
      FINISHED:
        my $rline_of_tokens = {};
        $rline_of_tokens->{_line_text}    = $input_stmt;
        $rline_of_tokens->{_line_number}  = $input_line_number;
        $rline_of_tokens->{_rtoken_types} = \@types;
        $rline_of_tokens->{_rtoken_map}   = \@token_map;
        $rline_of_tokens->{_rtokens}      = \@tokens;
        $rline_of_tokens->{_line_type}    = $stmt_type;
        $rline_of_tokens->{_lex_error}    = $lex_error;
        $rline_of_tokens->{_side_comment} = $side_comment;

        my $rmore_text = (@more_text) ? \@more_text : undef;
        return ( $rline_of_tokens, $rmore_text );
    }
}

sub show_tokens {

    # this is a debug routine
    my ( $rtokens, $rtoken_map ) = @_;
    my $num = scalar(@$rtokens);
    my $i;

    for ( $i = 0 ; $i < $num ; $i++ ) {
        my $len = length( $$rtokens[$i] );
        print "$i:$len:$$rtoken_map[$i]:$$rtokens[$i]:\n";
    }
}
1;
