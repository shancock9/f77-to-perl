#####################################################################
#
#    Fortran::F77toPerl converts Fortran 77 to perl
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
package Fortran::F77toPerl;
use 5.004;    # will need IO::File from 5.004 or later
#BEGIN { $^W = 1; }    # turn on warnings
use strict;
use warnings;
use Exporter;
use Carp;
$|++;
use vars qw{
  $VERSION
  @ISA
  @EXPORT
};

$VERSION = '1.0.0';
@ISA    = qw( Exporter );
@EXPORT = qw( &f2perl );

use Fortran::F77toPerl::Format;
use Fortran::F77toPerl::Lex77;
use Fortran::F77toPerl::Lister;
use Fortran::F77toPerl::SymbolTable;
use Fortran::F77toPerl::SourceReader;
use Fortran::F77toPerl::Debugger;
use Carp;

use constant DEBUG_FORMAT => 0;

sub f2perl {

    # translate one fortran file into a perl script
    my %input_hash = @_;

    my $filename = $input_hash{_source};
    my $destination  = $input_hash{_destination};

    unless ($filename) {
        croak "No input file given to f2perl\n";
    }
    unless ( ref($destination) eq 'SCALAR' ) {
        croak "f2perl expecting destination to be ref to SCALAR\n";
    }

    my $missing_perltidy;
    { eval "use Perl::Tidy"; $missing_perltidy = $@; }

    my $routput = process_file($filename);

    # add common block declarations, etc to the top of the output
    Fortran::F77toPerl::add_common_section($routput);

    # add a call to the main program at the end of the output
    my $main_program = Fortran::F77toPerl::get_main_program();
    if ($main_program) {

        # must use '&' form of call in case main program name is perl
        # keyword such as 'goto'
        push @$routput, "\&$main_program;\n";
    }

    # beautify output if possible
    unless ($missing_perltidy) {
        local @ARGV;

        # since the @$routput array contains multiple
        # lines per array element, we have to join them
        # and let perltidy re-separate them
        my $source = join "", @$routput;
        Perl::Tidy::perltidy(
            source      => \$source,
            destination => \@$routput,
        );
    }

    ${$destination} .= join "", @$routput;

}

sub process_file {

    my ($filename) = @_;
    my $reader     = Fortran::F77toPerl::SourceReader->new($filename);
    my $fileroot   = $filename;
    $fileroot =~ s/\.[^\.]$//;
    my $dot = ".";
    my $debugger;

    # uncomment if needed
    ##$debugger = Fortran::F77toPerl::Debugger->new( $fileroot . $dot . "DEBUG" );

    my @output;
    my $program_unit_count = 0;
    while (1) {
        my ( $rsubroutine, $rlabel_index ) = $reader->get_program_unit();
        last unless $rsubroutine;
        $program_unit_count++;
        my $routput =
          Fortran::F77toPerl::convert_program_unit( $program_unit_count,
            $rsubroutine, $rlabel_index, $debugger );
        push @output, @$routput;
    }
    return \@output;
}


{
    my (
        $program_unit_name, $program_unit_type,   $program_unit_count,
        $bad_line_count,    $filename,            $input_line,
        $input_line_number, $main_program,        $non_comment_count,
        $rlabel_index,      $rtokens,             $rtoken_types,
        $squeezed_text,     $stmt_signature,      $stmt_label,
        $stmt_text,         $stmt_type,           $local_symbol_table,
        $froze_storage,     $next_statement_text, $has_alternate_return,
        $need_IOlib,        $need_Format,
    );

    my (
        %is_function,                   %is_arg,
        %rcommon_variables,             %all_seen_common_names,
        %saw_statement_type,            %parameter_symbolic_value,
        %parameter_nonsymbolic_value,   %parameter_dependents,
        %is_parameter,                  %implicit_fortran_type,
        %implicit_fortran_type_default, %used_implicit_fortran_type,
        %saw_builtin,                   %is_main_arg,
        %saw_entry_arg,
    );

    my (
        %common_symbol_tables,
        %common_common_variable_name_order,
        %common_parameter_nonsymbolic_value,
        %common_parameter_symbolic_value,
        %common_parameter_name_order,
        %common_parameter_dependents,
        %common_block_maximum_size,
        %rlocal_common_equivalences,
    );

    BEGIN {
        @_ = qw(a b c d e f g h o p q r s t u v w x y z _);
        @implicit_fortran_type_default{@_} = ('real') x scalar(@_);
        @_                                 = qw(i j k l m n);
        @implicit_fortran_type_default{@_} = ('integer') x scalar(@_);
    }

    my ( @closing_braces, @parameter_name_order, @equivalences, @entry_points,
        @main_args, @alternate_return_labels );

    BEGIN { $bad_line_count = 0; }

    sub new_program_unit {
        clear_output();
        %rlocal_common_equivalences     = ();
        $program_unit_name              = "";
        $program_unit_type              = "";
        $non_comment_count              = 0;
        $froze_storage                  = 0;
        $squeezed_text                  = "";
        $stmt_signature                 = "";
        %is_arg                         = ();
        %is_main_arg                    = ();
        %rcommon_variables              = ();
        %saw_statement_type             = ();
        $saw_statement_type{executable} = 0;
        %parameter_symbolic_value       = ();
        %parameter_nonsymbolic_value    = ();
        %parameter_dependents           = ();
        %is_parameter                   = ();
        %implicit_fortran_type          = %implicit_fortran_type_default;
        %used_implicit_fortran_type     = ();
        @parameter_name_order           = ();
        @equivalences                   = ();
        @closing_braces                 = "";
        $local_symbol_table      = Fortran::F77toPerl::SymbolTable->new();
        $has_alternate_return    = 0;
        @main_args               = ();
        @entry_points            = ();
        %saw_entry_arg           = ();
        @alternate_return_labels = ();
    }

    # capitalize subs with conflicting names to avoid possible conflicts
    my %capitalize;

    BEGIN {
        @_ = qw(
          q
          qq
          qw
          qx
          qr
          s
          y
          tr
          m
          accept
          alarm
          bind
          binmode
          bless
          caller
          chdir
          chmod
          chomp
          chop
          chown
          chr
          chroot
          closedir
          cmp
          connect
          crypt
          dbmclose
          dbmopen
          defined
          delete
          die
          dump
          eof
          exec
          exists
          fcntl
          fileno
          flock
          formline
          getc
          getgrgid
          getgrnam
          gethostbyaddr
          gethostbyname
          getnetbyaddr
          getnetbyname
          getpeername
          getpgrp
          getpriority
          getprotobyname
          getprotobynumber
          getpwnam
          getpwuid
          getservbyname
          getservbyport
          getsockname
          getsockopt
          glob
          gmtime
          grep
          hex
          ioctl
          join
          keys
          kill
          lc
          lcfirst
          length
          link
          listen
          local
          localtime
          lock
          lstat
          map
          mkdir
          msgctl
          msgget
          msgrcv
          msgsnd
          oct
          opendir
          ord
          pack
          pipe
          pop
          pos
          printf
          prototype
          push
          quotemeta
          rand
          readdir
          readlink
          readline
          readpipe
          recv
          ref
          rename
          require
          reset
          reverse
          rewinddir
          rindex
          rmdir
          scalar
          seek
          seekdir
          select
          semctl
          semget
          semop
          send
          sethostent
          setnetent
          setpgrp
          setpriority
          setprotoent
          setservent
          setsockopt
          shmctl
          shmget
          shmread
          shmwrite
          shutdown
          sleep
          socket
          socketpair
          sort
          splice
          split
          sprintf
          srand
          stat
          study
          substr
          symlink
          syscall
          sysopen
          sysread
          sysseek
          syswrite
          tell
          telldir
          tie
          tied
          truncate
          uc
          ucfirst
          umask
          undef
          unlink
          unpack
          unshift
          untie
          utime
          values
          vec
          waitpid
          warn
          xor
          endgrent
          endhostent
          endnetent
          endprotoent
          endpwent
          endservent
          fork
          getgrent
          gethostent
          getlogin
          getnetent
          getppid
          getprotoent
          getpwent
          getservent
          setgrent
          setpwent
          time
          times
          wait
          wantarray

          sub
          package
          bless

          and
          close
          continue
          each
          else
          elsif
          eq
          exit
          for
          foreach
          ge
          goto
          gt
          if
          last
          le
          local
          lt
          my
          ne
          next
          no
          not
          open
          or
          our
          print
          read
          redo
          return
          shift
          unless
          until
          use
          while
          write
        );
        foreach my $word (@_) { $capitalize{$word} = uc($word) }

        # keywords for which no capitilization is done
        my @tbd = qw(
          abs
          atan2
          cos
          exp
          index
          int
          log
          sin
          sqrt
          system
        );
    }

    my %CAPITALIZED;

    sub capitalization_check {
        my $word = shift;
        if ( $capitalize{$word} ) {
            $word = $capitalize{$word};
            $CAPITALIZED{$word} = 1;
        }
        return $word;
    }

    # define some fortran functions which do not exist in perl
    my %builtin_code;

    BEGIN {
        %builtin_code = (
            'dble' => <<'EOM',
    sub dble {  $_[0] }
EOM
            'index_F77' => <<'EOM',
    sub index_F77 { 1 + index( $_[0], $_[1] ) }
EOM
            'llt_F77' => <<'EOM',
    sub llt_F77 {  ($_[0] cmp $_[1] ) < 0 }
EOM
            'lgt_F77' => <<'EOM',
    sub lgt_F77 {  ($_[0] cmp $_[1] ) > 0 }
EOM
            'tan' => <<'EOM',
    sub tan {  sin($_[0])/cos($_[0]); }
EOM
            'second' => <<'EOM',
    sub second {
        use Time::HiRes qw(gettimeofday);
        return gettimeofday();
    }
EOM
            'sign' => <<'EOM',
    sub sign {
        my ($v,$s)=@_;
        if (!defined $s) {return $v >= 0 ? 1 : -1}
	return $s>=0 ? abs($v):-abs($v);
    }
EOM
            'max' => <<'EOM',
    sub max {
        my $max = shift;
        foreach (@_) {
            $max = ( $max < $_ ) ? $_ : $max;
        }
        return $max;
    }
EOM
            'min' => <<'EOM',
    sub min {
        my $min = shift;
        foreach (@_) {
            $min = ( $min > $_ ) ? $_ : $min;
        }
        return $min;
    }
EOM
            'eqv' => <<'EOM',
    sub eqv { $_[0] && $_[1] || !$_[0] && !$_[1] }
}
EOM
            'neqv' => <<'EOM',
    sub neqv { $_[0] && !$_[1] || !$_[0] && $_[1] }
EOM
            'log10' => <<'EOM',
    sub log10 { log( $_[0] ) / log(10); }
EOM
            'PAUSE' => <<'EOM',
    sub PAUSE { 
          print "PAUSE  ", $_[0], " statement executed\n"; 
          print "To resume execution, type go.  Other input will terminate the job.\n";
          my $ans = <>; 
          unless ($ans =~ /^go$/i) {
               die "STOP";
          };
    }
EOM
        );
    }

    sub add_common_section {
        my ($routput) = @_;

        my $rcommon_blocks = get_common_block_packages();
        if (@$rcommon_blocks) {
            unshift @$routput, @$rcommon_blocks;
            unshift @$routput, "use Fortran::F77toPerl::Common;\n";
        }

        my $lib_module_usage = get_lib_module_usage();
        if (@$lib_module_usage) {
            unshift @$routput, @$lib_module_usage;
        }

        my $conversion_functions = get_conversion_functions();
        if (@$conversion_functions) {
            unshift @$routput, @$conversion_functions;
        }

        my $notes = get_notes();
        if (@$notes) {
            unshift @$routput, @$notes;

            unshift @$routput, q($^W=1;) . "\n";
            unshift @$routput, "use strict;\n";
        }
    }

    sub get_lib_module_usage {
        my @use;
        if ($need_IOlib) {
            push @use, "use Fortran::F77toPerl::IOlib;\n";
            push @use, "our \$IOlib = Fortran::F77toPerl::IOlib->new();\n";
            push @use, "\n";
            push @use, "# pre-connect units 5 and 6 to stdin and stdout\n";
            push @use, "\$IOlib->open(unit=>5,file=>'-',mode=>'r');\n";
            push @use, "\$IOlib->open(unit=>6,file=>'-',mode=>'w');\n";
        }
        if ($need_Format) {
            push @use, "use Fortran::F77toPerl::Format;\n";
        }
        return wantarray ? @use : \@use;
    }

    sub get_conversion_functions {
        my @subs;
        foreach ( keys %builtin_code ) {
            if ( $saw_builtin{$_} ) {
                push @subs, $builtin_code{$_};
            }
        }
        return wantarray ? @subs : \@subs;
    }

    sub get_notes {

        # create any warning comments to go at the top of the output
        my @words = sort keys %CAPITALIZED;
        my @notes;
        if (@words) {
            my $s = ( @notes == 1 ) ? "" : 's';
            push @notes, <<EOM;

# -------------------------------------------------------------
# function name$s capitalized to avoid collisions with builtins:
EOM
            push @notes, "# " . ( join ", ", @words ) . "\n";
            push @notes, <<EOM;
# -------------------------------------------------------------

EOM
        }
        if ( $saw_builtin{'length'} ) {
            my $count = $saw_builtin{'length'};
            my $s     = ( $count > 1 ) ? 's' : "";
            my $msg   = <<EOM;
# -------------------------------------------------------------
# The Fortran 'len' function has been mapped into 'length' in $count place$s,
# so please carefully check all occurances of 'length' to see if this is ok.
# -------------------------------------------------------------
EOM
            push @notes, $msg;
            push @notes, "\n";
            print STDERR $msg;
        }
        if ( $saw_builtin{'eq'} ) {
            my $count = $saw_builtin{'eq'};
            my $s     = ( $count > 1 ) ? 's' : "";
            my $msg   = <<EOM;
# -------------------------------------------------------------
# The perl 'eq' and 'ne' string comparison operators are used in $count place$s.
# Please check the code because unlike Fortran they are sensitive to blanks.
# -------------------------------------------------------------
EOM
            push @notes, $msg;
            push @notes, "\n";
            print STDERR $msg;
        }
        return wantarray ? @notes : \@notes;
    }

    sub get_storage_type {
        my ($token) = @_;
        return $local_symbol_table->get( $token, 'storage_type' );
    }

    sub get_common_block_packages {

        my @common_blocks = ();
        my @keys          = sort keys %all_seen_common_names;
        foreach my $common_name ( sort keys %all_seen_common_names ) {
            my $rcommon_block = get_common_block_package($common_name);
            push @common_blocks, @$rcommon_block;
        }
        if (@common_blocks) {
            push @common_blocks, "\n";
            push @common_blocks, "package main;\n";
        }
        return wantarray ? @common_blocks : \@common_blocks;
    }

    sub get_common_block_package {

        # create the code which generates common block $common_name
        my ($common_name) = @_;
        my %is_needed_parameter = ();

        # ----------------------------------------------
        # First create the inner BEGIN { } block
        # ----------------------------------------------
        my @BEGIN_block;
        my $add_line = sub {
            my $line = shift;
            push @BEGIN_block, $line . "\n";
        };

        $add_line->(<<'EOM');
    BEGIN {
        our $COMMON   = [];
        our %Offset;
        no strict 'vars';
EOM
        my $size = $common_block_maximum_size{$common_name};
        if ($size) {
            $add_line->(<<"EOM");
        # pin down common size to keep perl from relocating the array
        \$COMMON->[$size]=0;
EOM
        }
        $add_line->(<<'EOM');

        # make the common block maker
        my (
            $push_scalars, $push_array, $push_array_min1,
            $declare_array, $declare_array_min1, $equivalence
          );
        eval($Fortran::F77toPerl::Common::block_maker);
        if ($@) {die "error eval'ing Common Block Code::$@\n";}

        # make the common variables in the proper order
EOM

        # routine to create an array declaration line
        # the leading sub call of the form 'push_array->(...
        my $get_push_array_line = sub {
            my ( $common_name, $name ) = @_;
            my $line;
            my $line_min1;
            my $min1_ok = 1;
            my $rimins  =
              $common_symbol_tables{$common_name}->get( $name, 'imins' );
            my $rimaxs =
              $common_symbol_tables{$common_name}->get( $name, 'imaxs' );
            my $dependencies =
              $common_symbol_tables{$common_name}->get( $name, 'dependencies' );
            if ($dependencies) {

                foreach (@$dependencies) {
                    $is_needed_parameter{$_} = 1;
                }
            }

            # form both formats and return the _min1 if all mins are 1
            $line      = "\$push_array->('$name'";
            $line_min1 = "\$push_array_min1->('$name'";
            foreach my $n ( 0 .. @$rimaxs - 1 ) {
                my $imin = $rimins->[$n];
                my $imax = $rimaxs->[$n];
                $imin =~ s/\$//g;    # params have leading '$'
                $imax =~ s/\$//g;
                $line      .= ", $imin, $imax";
                $line_min1 .= ", $imax";
                $min1_ok = 0 if ( $imin ne '1' );
            }
            $line      .= ');';
            $line_min1 .= ');';
            return $min1_ok ? $line_min1 : $line;
        };

        my %installed = ();
        my $scalar_list;
        foreach
          my $name ( @{ $common_common_variable_name_order{$common_name} } )
        {
            $installed{$name} = 1;
            my $storage_type =
              $common_symbol_tables{$common_name}->get( $name, 'storage_type' );
            if ( $storage_type eq '$' ) {
                $scalar_list .= "'$name', ";
            }
            else {
                if ($scalar_list) {
                    $scalar_list =~ s/, $//;
                    $add_line->("\$push_scalars->($scalar_list);");
                    $scalar_list = "";
                }
                my $line = $get_push_array_line->( $common_name, $name );
                $add_line->($line);
            }
        }
        if ($scalar_list) {
            $scalar_list =~ s/, $//;
            $add_line->("\$push_scalars->($scalar_list);");
            $scalar_list = "";
        }

        my @symbols = ( $common_symbol_tables{$common_name}->get_symbols() );
        my @equivalences;
        foreach my $lname (@symbols) {
            my $cname =
              $common_symbol_tables{$common_name}->get( $lname, 'common_name' );
            if ( $cname && $cname eq $common_name ) {
                my $rname =
                  $common_symbol_tables{$common_name}
                  ->get( $lname, 'equivalence_to' );
                next unless ($rname);
                my $rindex =
                  $common_symbol_tables{$common_name}
                  ->get( $lname, 'equivalence_rindex' );
                my $lindex =
                  $common_symbol_tables{$common_name}
                  ->get( $lname, 'equivalence_lindex' );
                my $storage_type =
                  $common_symbol_tables{$common_name}
                  ->get( $lname, 'storage_type' );
                unless ($storage_type) {
                    print STDERR
                      "Storage type not defined for equivalent $lname\n";
                    next;
                }
                if ( $storage_type eq '@' ) {
                    my $line = $get_push_array_line->( $common_name, $lname );
                    $line =~ s/^\$push/\$declare/;
                    $add_line->($line);
                }
                push @equivalences, [ $lname, $lindex, $rname, $rindex ];
            }
        }

        # make the equivalences
        my $more_to_do = 1;
        my $pass       = @equivalences;
        while ($more_to_do) {
            $more_to_do = 0;

            # shouldn't happen
            if ( $pass-- < 0 ) {
                print STDERR "Program bug resolving equivalences\n";
                last;
            }
            my $count = @equivalences;
            foreach my $i ( 0 .. @equivalences - 1 ) {
                my ( $lname, $lindex, $rname, $rindex ) =
                  @{ $equivalences[$i] };
                next unless ( $installed{$rname} );
                $installed{$lname} = 1;

                my $line = "\$equivalence->(['$lname'";
                if ( defined($lindex) ) {
                    $line .= "," . join( ',', @$lindex );
                }
                $line .= "],['$rname'";
                if ( defined($rindex) ) {
                    $line .= "," . join( ',', @$rindex );
                }
                $line .= "]);";
                $add_line->($line);

                splice @equivalences, $i, 1;
                $more_to_do = 1 if @equivalences;
                last;
            }
        }

        # shouldn't happen:
        if (@equivalences) {
            my $msg = "Error making these common equivalences: @equivalences\n";
            $add_line->("## $msg");
        }

        $add_line->(<<'EOM');
    }
EOM

        # --------------------------
        # Now create the outer code
        # --------------------------
        my @outer_block;
        $add_line = sub {
            my $line = shift;
            push @outer_block, $line . "\n";
        };

        $add_line->(<<"EOM");
    
    # declarations for common /$common_name/
    
    package $common_name;
EOM

        # gather all parameter dependencies
        my @todo = keys %is_needed_parameter;
        while ( my $param = pop @todo ) {
            my $ref = $common_parameter_dependents{$common_name}->{$param};
            next unless $ref;
            my @deps = @$ref;
            foreach (@deps) {
                unless ( $is_needed_parameter{$_} ) {
                    $is_needed_parameter{$_} = 1;
                    push @todo, $_;
                }
            }
        }

        # declare parameters as constants here
        foreach my $param ( @{ $common_parameter_name_order{$common_name} } ) {
            if ( $is_needed_parameter{$param} ) {
                my $value =
                  $common_parameter_symbolic_value{$common_name}->{$param};
                $add_line->("use constant $param => $value;");
            }
        }
        my @common_block = ( @outer_block, @BEGIN_block );
        return wantarray ? @common_block : \@common_block;
    }

    sub get_main_program {
        return ($main_program);
    }

    sub warning {
        my ($msg) = @_;
        errmsg( "Warning: " . $msg );
    }

    sub error {
        my ($msg) = @_;
        errmsg( "Error: " . $msg );
    }

    sub errmsg {
        my ($msg) = @_;
        my $truncated_line = substr( $input_line, 0, 72 );
        if ( length($input_line) > length($truncated_line) ) {
            $truncated_line .= '...';
        }
        print STDERR <<EOM;
$input_line_number : $truncated_line
$msg
EOM
    }

    sub unrecognized {
        my $msg = "** unrecognized form of '$_[0]' statement:\n";
        error($msg);
        write_comment("## $input_line");
    }

    sub not_implemented {
        error("$stmt_type is not implemented\n");
        write_comment("## $input_line");
    }

    sub scan_args {

        # scan subroutine or function arg list
        my ($args) = @_;
        my @args;
        if ($args) {
            @args = split_args($args);
            @is_arg{@args} = (1) x scalar(@args);
            unless ( $stmt_type eq 'entry' ) {
                @is_main_arg{@args} = (1) x scalar(@args);
            }
        }
        return wantarray ? @args : \@args;
    }

    sub check_label {

        # 1. clean up label by removing any leading zeros
        # 2. look up the line number of this label
        # 3. return cleaned up label and the line number
        my ($label) = @_;
        $label =~ s/^0+//;
        my $line_number = $rlabel_index->{$label};
        unless ($line_number) {
            error("check_label: did not see format label: $label\n");
            write_comment("## ERROR undefined label: $label");
        }
        return ( $label, $line_number );
    }

    sub get_goto_label {
        my ($label) = @_;
        ( $label, my $line_number ) = check_label($label);
        $label = "L$label";
        if ($line_number) {
            mark_target("$label:");
        }
        else {
            write_comment("## ERROR undefined label: $label");
        }
        return $label;
    }

    sub set_implicit_fortran_type {
        my ( $ic, $type ) = @_;
        my $old_type = $implicit_fortran_type{$ic};
        my $success  = 1;
        if ( $used_implicit_fortran_type{$ic} ) {
            if ( defined($old_type) && $type ne $old_type ) {
                $success = 0;
                error(
                    "implicit stmt appears too late, letter $ic already used\n"
                );
            }
        }
        $implicit_fortran_type{$ic} = $type;
        return $success;
    }

    sub letter_range {

        # store the type for a letter range of the form 'a-h'
        my ( $type, $term ) = @_;
        my ( $first, $last ) = split /-/, $term;
        if ( $first !~ /^[a-z]$/ ) {
            error("error in initial letter '$first' \n");
            return;
        }

        if ( !defined($last) ) {
            set_implicit_fortran_type( $first, $type );
        }
        else {
            if ( $last !~ /^[a-z]$/ ) {
                error("error in ending letter '$last' \n");
                return;
            }
            my $ib = ord($first);
            my $ie = ord($last);
            if ( $ie < $ib ) {
                error("improper order from $first to $last\n");
                return;
            }
            foreach my $i ( $ib .. $ie ) {
                my $ic = chr($i);
                set_implicit_fortran_type( $ic, $type );
            }
        }
    }

    sub write_io_return_goto_code {

        my ( $ERR, $err ) = @_;
        return unless $err;
        $err = get_goto_label($err);
        write_code("goto $err if \$$ERR;");
    }

    sub make_io_return_variables {
        foreach (@_) {
            $local_symbol_table->set(
                'name'              => $_,
                'storage_type'      => '$',
                'save'              => 0,
                'fortran_data_type' => 'integer',
            );
        }
    }

    sub split_csv_list {

        # get the token index limits of all the comma-separated terms within
        # a list.  The token range of interest is $ibeg .. $iend
        my ( $ibeg, $iend ) = @_;
        my @ii_commas = find_commas( $ibeg, $iend );
        my @terms;
        my $ib;
        my $ie = $ibeg - 2;
        for ( my $ic = 0 ; $ic <= @ii_commas ; $ic++ ) {
            $ib = $ie + 2;
            if ( $ic == @ii_commas ) { $ie = $iend }
            else { $ie = $ii_commas[$ic] - 1 }

            # ignore a leading comma
            # for example, ignore the comma before 'temp' here:
            # read (*,*),temp
            if ( $ie >= $ib ) {
                push @terms, [ $ib, $ie ];
            }
        }
        return wantarray ? @terms : \@terms;
    }

    sub join_tokens {
        my ( $ib, $ie ) = @_;
        return join "", @$rtokens[ $ib .. $ie ];
    }

    sub index_token_types {
        my ( $tok, $ib, $ie ) = @_;
        my $imatch;
        foreach my $i ( $ib .. $ie ) {
            if ( $rtoken_types->[$i] eq $tok ) { $imatch = $i; last }
        }
        return $imatch;
    }

    sub scan_control_list {

        # parse an io control list of name=value pairs; for example
        #      (unit=5,  format='(a)')
        my ( $ib, $ie, @defaults ) = @_;

        # ($ib,$ie) are the token index range to parse
        # @defaults are positional dependent defaults in case the
        # name is missing
        my @items = split_csv_list( $ib, $ie );
        my @pairs;
        my $count = -1;

        # loop to examine each list item
        foreach my $item (@items) {
            my ( $ibeg, $iend ) = @$item;
            $count++;

            my ( $name, $value );
            my $i_equals = index_token_types( '=', $ibeg, $iend );
            my $iv_beg   = $ibeg;
            my $iv_end   = $iend;
            if ($i_equals) {
                $name   = join_tokens( $ibeg, $i_equals - 1 );
                $iv_beg = $i_equals + 1;
                $value  = join_tokens( $i_equals + 1, $iend );
            }
            else {
                $value = join_tokens( $ibeg, $iend );
                if ( $count <= @defaults ) {
                    $name = $defaults[$count];
                }
                else {
                    error(" near '$value'\n");
                    $name = 'UNKNOWN';
                }
            }

            # store the name and value, as well as the token index range
            push @pairs, [ $name, $value, $iv_beg, $iv_end ];
        }
        return wantarray ? @pairs : \@pairs;
    }

    sub make_io_code {

        # make code for a 'read', 'write', or 'print' statement

        my @pairs;
        my $max_index = @$rtokens - 1;
        my $i_closing_paren;

        # first part of 'print', and older style 'read' and 'write':
        #    print *, i, j
        #    read 1000, i, j
        #    write 2000, i, j
        if ( $rtokens->[1] ne '(' ) {
            my $format_type;
            my $format;
            my ( $i_comma, $comma ) = find_next_comma(1);
            my $iv_beg = 1;
            my $iv_end = $max_index;
            if ( $comma eq ',' ) {
                $format = join_tokens( 1, $i_comma - 1 );
                $iv_end = $i_comma - 1;
            }
            else {
                $format = join_tokens( 1, $max_index );
            }
            $i_closing_paren = $i_comma;
            push @pairs, [ 'fmt', $format, $iv_beg, $iv_end ];
            push @pairs, [ 'unit', '*', undef, undef ];
        }

        # first part of normal 'read' and 'write'
        # read (6,1000)
        else {
            my $i_open_paren = 1;
            $i_closing_paren = find_closing_paren($i_open_paren);
            @pairs           = scan_control_list(
                $i_open_paren + 1,
                $i_closing_paren - 1,
                'unit', 'fmt'
            );
        }

        my $call_hash;
        if ( $stmt_type eq 'read' ) {
            $call_hash = "F77_read(";
        }
        else {
            $call_hash = "F77_write(";
        }
        my ( $format, $unit, $end, $err );
        my $use_sprintf;
        my ( $iv_beg_unit, $iv_end_unit );

        foreach my $term (@pairs) {
            my ( $name, $value, $iv_beg, $iv_end ) = @$term;
            if ( $name eq 'iostat' ) {
                $value = '\\' . $value;
            }
            elsif ( $name eq 'unit' ) {
                $unit        = $value;
                $iv_beg_unit = $iv_beg;
                $iv_end_unit = $iv_end;
            }
            elsif ( $name eq 'end' ) {
                $end   = $value;
                $value = '\$END';
            }
            elsif ( $name eq 'err' ) {
                $err   = $value;
                $value = '\$ERR';
            }
            elsif ( $name eq 'fmt' ) {
                $format = $value;
                if ( $value =~ /^\d+$/ ) {
                    ( $value, my $line_number ) = check_label($value);
                    $value  = '$fmt_' . $value;
                    $format = $value;
                }
                elsif ( $value eq '*' ) { $value = "'*'" }
                else {

                    ## patch until real parsing is done:
                    if ( $value =~ /^(['"])/ || $value =~ /^q\@/ ) {
                        my $fmt = eval($value);
                        ( $value, my $count ) =
                          Fortran::F77toPerl::Format::convert_to_perl($fmt);
                    }
                    else {
                        $need_Format=1;
                        $value =
"Fortran::F77toPerl::Format::convert_to_printf($value)";
                    }
                }
                $format = $value;
            }
            if ( $value eq '*' ) { $value = "'*'" }
            $call_hash .= "$name => $value,";
        }

        my $error            = 0;
        my $rlist             = [];
        my $list_obj         = new Fortran::F77toPerl::Lister( $rlist );
        my $depth            = 0;
        my @name_list_output = ();
        my @simple_list      = ();
        my $simple_list_ok   = 1;
        my $shift_mode;

        if ( $stmt_type eq 'read' ) {
            $shift_mode = "shift \@\$RIO_LIST";
        }
        else {
            $shift_mode = "push \@\$RIO_LIST, ";
        }

        my $item_count = 0;
        my $simple_list;
        if ( $i_closing_paren < $max_index ) {
            $item_count = scan_name_list(
                $depth,               $list_obj,
                $i_closing_paren + 1, $max_index,
                \$error,              \$shift_mode,
                \@name_list_output,   \@simple_list,
                \$simple_list_ok
            );

            $simple_list_ok = 0 unless @simple_list;

            if ($item_count) {
                if ( $stmt_type eq 'read' ) {
                    write_code('$RIO_LIST=[];');
                    $call_hash .= "count => $item_count,";
                    $call_hash .= 'iolist => $RIO_LIST,';
                    make_io_return_variables('RIO_LIST');
                }
                else {
                    unless ($simple_list_ok) {
                        write_code('$RIO_LIST=[];');
                        $call_hash .= 'iolist => $RIO_LIST,';
                        make_io_return_variables('RIO_LIST');
                    }
                }
            }
        }

        # convert fortran logical unit to a filehandle if necessary
        my $unit_is_string;
        if ( $unit eq '*' ) {
            $unit = "";
        }
        elsif ( defined($iv_beg_unit) ) {

            # check for write to string
            if ( $rtokens->[$iv_beg_unit] eq 'substr' ) {
                $unit_is_string = 1;
            }
            else {
                my $fortran_data_type =
                  $local_symbol_table->get( $rtokens->[$iv_beg_unit],
                    'fortran_data_type' );
                if ( $fortran_data_type && $fortran_data_type eq 'character' ) {
                    $unit_is_string = 1;
                }
            }

            unless ($unit_is_string) {
                my $FH_name = 'Fh';   # FH is ok too unless used for a parameter
                $local_symbol_table->set(
                    'name'         => $FH_name,
                    'save'         => 0,
                    'storage_type' => '$',
                );
                $FH_name    = '$' . $FH_name;
                $need_IOlib = 1;
                my $mode = ( $stmt_type eq 'read' ) ? 'r' : 'w';
                write_code("$FH_name=\$IOlib->FH($unit,'$mode');");
                $unit = $FH_name;
            }
        }
        else {
            error("logical unit has not been specified");
            $unit = "";
        }
        $call_hash .= ");";

        # handle list-directed and formatted write
        if ( $stmt_type ne 'read' && $format ) {

            ## TODO: add error checks
            my $iolist = "";
            my $code = "";

            # list-directed write
            if ( $format eq "'*'" ) {

                if ( $simple_list_ok ) {
                    $iolist = join( ",'  ',", @simple_list );
                }
                elsif (@name_list_output) {
                    foreach (@name_list_output) { write_code($_) }
                    $iolist = q(join '  ', @$RIO_LIST);
                }

                if ($unit_is_string) { $code = "$unit = " }
                else { $code = "print $unit"; }
                if ($iolist) {
                    $code .= $iolist;
                    $code .= ', ';
                }
                $code .= '"\n"' unless $unit_is_string;
            }

            # formatted write
            else {
                if ( $simple_list_ok ) {
                     $iolist = join( ",", @simple_list );
                }
                elsif (@name_list_output) {
                    foreach (@name_list_output) { write_code($_) }
                    $iolist = '@$RIO_LIST';
                }
                if ($unit_is_string) { $code = "\$_=sprintf $format" }
                else { $code = "printf $unit $format" }
                if ($iolist) {
                    $code .= ', ';
                    $code .= $iolist;
                }
            }

            # we should remove any terminal newline for a print to string
            if ( $unit_is_string && $format ne "'*'" ) {
                $code .= "; chomp; $unit=\$_";
            }

            write_code( $code . ';' );
        }

        # Handle list-directed read
        elsif ($stmt_type eq 'read'
            && $format
            && $format eq "'*'" )
        {
            my $iolist = "";
            if ($simple_list_ok) {
                $iolist = '(' . join( ", ", @simple_list ) . ')';
            }
            else {
                $iolist = '@$RIO_LIST';
            }
            if ($unit_is_string) {
                write_code("$iolist = split ' ',$unit;");
            }
            else { write_code("$iolist = split ' ',<$unit>;"); }
            unless ($simple_list_ok) {
                if (@name_list_output) {
                    foreach (@name_list_output) { write_code($_) }
                }
            }
        }

        # Handle all other I/O: these are left as unimplemented
        # library calls for now
        else {

            if ( $err || $end ) {
                make_io_return_variables( 'ERR', 'END' );
            }

            # write code to fill the io buffer for a write
            if ( $stmt_type ne 'read' ) {
                foreach (@name_list_output) { write_code($_) }
            }

            # write code to do the read or write
            write_code($call_hash);
            write_io_return_goto_code( 'ERR', $err ) if $err;
            write_io_return_goto_code( 'END', $end ) if $end;

            # write code to get values from the io buffer for a read
            if ( $stmt_type eq 'read' ) {
                foreach (@name_list_output) { write_code($_) }
            }
        }
    }

    sub quote_string {

        # Given a string, make it a perl quoted string
        my ($str) = @_;

        # must escape backslashes.
        # Otherwise, for example, this would fail:
        #    if (i.eq.'\') then
        $str =~ s/\\/\\\\/g;

        # First try to use single quotes
        if ( $str !~ /\'/ ) {
            $str = "'" . $str . "'";
            return $str;
        }

        # If no success, use the q operator.
        # Try to find a unique quote character
        my $char;
        if    ( $str !~ /\@/ ) { $char = '@'; }
        elsif ( $str !~ /\!/ ) { $char = '!'; }
        elsif ( $str !~ /\#/ ) { $char = '#'; }
        elsif ( $str !~ /\%/ ) { $char = '%'; }
        elsif ( $str !~ /\|/ ) { $char = '|'; }
        elsif ( $str !~ /\~/ ) { $char = '~'; }
        elsif ( $str !~ /\^/ ) { $char = '^'; }
        elsif ( $str !~ /\&/ ) { $char = '&'; }
        elsif ( $str !~ /\\/ ) { $char = '\\'; }
        else {

            # FIXME: should really see if the @ is quoted
            $char = '@';
            $str =~ s/\@/\\@/g;
        }
        $str = 'q' . $char . $str . $char;
        return $str;
    }

    sub split_args {

        # given a list of sub or function call args, break out the individual
        # args and check for alternate returns
        my $args = shift;
        my @args;
        if ($args) {
            $args =~ s/^\(//;
            $args =~ s/\)$//;
            my @raw_args = split ',', $args;
            @args = grep { $_ ne '*' } @raw_args;
            $has_alternate_return |= ( @raw_args - @args );
            if ( $has_alternate_return && $program_unit_type ne 'subroutine' ) {
                warning("alternate return only valid in subroutine\n");
            }
        }
        return wantarray ? @args : \@args;
    }

    sub make_entry_code {

        # make the entry point subs from saved entry statements
        foreach my $entry (@entry_points) {
            my ( $entry_name, $rargs ) = @$entry;
            my $Entry_name = uc $entry_name;
            write_code("sub $entry_name\{");
            if ($rargs) {
                make_call_arg_declarations( 1, @$rargs );
            }
            write_code("    \$Entry=\"$Entry_name\";");
            write_code("    $program_unit_name();");
            write_code("}");
        }
    }

    # ----------------------------------------------------------
    # hash table of code to handle the different statement types
    # ----------------------------------------------------------

    # These code sections may use any of the following:
    #    $rtokens,       - reference to the final array of tokens
    #    $rtoken_types   - reference to the final array of token types
    #    $squeezed_text  - is join of all of the tokens in $rtokens
    #    $stmt_signature - is the join of all of the types in $rtoken_types
    # They write any code out with the appropriate 'write_xxxx' routines

    my $tokenization_code = {

        'assign' => sub {

            my $max_token_index = @$rtokens - 1;
            if ( $max_token_index eq 3 ) {
                my $target = $rtokens->[1];
                my $to     = $rtokens->[2];
                my $var    = $rtokens->[3];
                if ( $to eq 'to' && $target =~ /^\d+$/ ) {
                    my $label = get_goto_label($target);
                    write_code("$var = \"$label\";");
                }
                else {
                    unrecognized($stmt_type);
                }
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'backspace' => sub {
            shift @$rtokens;
            my $lun = join '', @$rtokens;
            if ($lun) {
                not_implemented();
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'dimension' => sub { },

        'character' => sub { },

        'common' => sub { },

        'doubleprecision' => sub { },

        'real' => sub { },

        'integer' => sub { },

        'byte' => sub { },

        'logical' => sub { },

        'blockdata' => sub {

            if ( $squeezed_text =~ /^blockdata(\w+)?$/ ) {

                $program_unit_type = 'blockdata';
                my $line = 'blockdata';
                if ($1) {
                    $program_unit_name = $1;
                    $line .= " $1";
                }
                else {
                    $program_unit_name = 'blockdata';
                }
                write_outer_block_name("$line");
            }
        },

        'call' => sub {
            if ( $squeezed_text =~ /^call(.+)$/ ) {
                my $code = $1;
                if ( $code !~ /\(/ ) { $code .= '()' }
                if (@alternate_return_labels) {
                    $code = '$_ = ' . $code;
                    write_code("$code;");
                    my $olabels = join " ", @alternate_return_labels;
                    my $count = @alternate_return_labels;
                    if ( $count == 1 ) {
                        write_code("goto $olabels if \$_ == $count;");
                    }
                    else {
                        write_code(
"goto qw($olabels )[\$_-1] if (\$_ >0 && \$_ <= $count);"
                        );
                    }
                }
                else {
                    write_code("$code;");
                }
            }
            else {
                unrecognized('call');
            }
            @alternate_return_labels = ();
        },

        'close' => sub {

            if ( $squeezed_text =~ /^close\(.*\)$/ ) {
                my $max_index    = @$rtokens - 1;
                my $i_open_paren = 1;
                my @pairs        =
                  scan_control_list( $i_open_paren + 1, $max_index - 1,
                    'unit' );
                $need_IOlib = 1;
                my $call_hash = "\$IOlib->close(";
                foreach my $term (@pairs) {
                    my ( $name, $value, $iv_beg, $iv_end ) = @$term;
                    if ( $name eq 'iostat' ) { $value = '\\' . $value }
                    $call_hash .= "$name => $value,";
                }
                $call_hash .= ");";
                write_code($call_hash);
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'complex' => sub {
            not_implemented();
        },

        'continue' => sub {
            unless ( $stmt_signature eq 'k' ) {
                unrecognized($stmt_type);
            }
        },

        # nonstanard for f77
        'cycle' => sub {
            unless ( $stmt_signature eq 'k' ) {
                unrecognized($stmt_type);
            }
            write_code("next;");
        },

        'data' => sub { },

        'decode' => sub {
            not_implemented();
        },

        'do' => sub {

            # First let's change any commas at highest level to some
            # non-fortran character (#) to allow parsing with a regex
            my $i_comma = find_next_comma(0);
            while ( $rtokens->[$i_comma] eq ',' ) {
                $rtokens->[$i_comma] = '#';
                $i_comma = find_next_comma( $i_comma + 1 );
            }

            # now stick it all together
            my $masked_stmt = join "", @$rtokens;

            # Examples:
            #     Original:           Masked:
            #     DO 55 I=1,100,2     DO55I=1#100#2
            #     DO N=1,100          DON=1#100
            #     DO J=1,LIM(K,L)     DOJ=1#LIM(K,L)
            #
            # Note: a comma after the statement label is commonly allowed
            if ( $masked_stmt =~
                /^do((\d+)?#?)(\$[\w:]+)=([^#]+)#([^#]+)(#([^#]+))?$/ )
            {
                my $target = $2;
                my $var    = $3;
                my $start  = $4;
                my $end    = $5;
                my $inc    = defined($7) ? $7 : 1;
                if ($inc=~/^\d+$/) {
                   write_code("for ($var=$start; $var<=$end; $var+=$inc) {");
                }
                elsif ($inc=~/^-(\d+)$/) {
                   write_code("for ($var=$start; $var>=$end; $var-=$1) {");
                }
                else {
                   write_code(
"for ($var=$start; $inc < 0 ? $var >= $end : $var<=$end; $var+=$inc) {"
                   );
                }
                if ( defined($target) ) {
                    ( $target, my $line_number ) = check_label($target);
                    if ($line_number) {
                        $closing_braces[$line_number] .= '}';
                    }
                    else {
                        unrecognized('do');
                    }
                }
            }
            elsif ( $squeezed_text eq 'do' ) {
                write_code("while (1) {");
            }
            else {
                unrecognized('do');
                print STDERR "$masked_stmt\n";
            }
        },

        # nonstanard for f77
        'dowhile' => sub {
            if ( $squeezed_text =~ /^dowhile(\(.+\))$/ ) {
                my $expr = $1;
                write_code("while $expr {");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'else' => sub {
            if ( $stmt_signature eq 'k' ) {
                write_code("} else {");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'elseif' => sub {
            if ( $squeezed_text =~ /^elseif(\(.+\))then$/ ) {
                my $expr = $1;
                write_code("} elsif $expr {");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'encode' => sub {
            not_implemented();
        },

        'enddo' => sub {
            if ( $stmt_signature eq 'k' ) {
                write_code("}");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'endif' => sub {
            if ( $stmt_signature eq 'k' ) {
                write_code("}");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'end' => sub {
            if ( $stmt_signature ne 'k' ) {
                warning("extra text after 'end' ignored\n");
            }

            # create the return code...
            write_label("RETURN:");

            # NOTE: uncomment to force this return label to be kept
            # to allow easy code maintenance
            # mark_target("RETURN:");

            if ( $program_unit_type eq 'function' ) {
                my $lc_name = lc $program_unit_name;
                write_code("return \$$lc_name;");
            }
            elsif ( $program_unit_type eq 'subroutine' ) {
                write_code("return;");
            }

            if ( $program_unit_type && $program_unit_type ne 'blockdata' ) {

                # output the closing brace of this subroutine
                write_code("}");

                # output any entry statement code before the final closing brace
                if (@entry_points) {
                    make_entry_code();
                    $local_symbol_table->set(
                        'name'              => 'Entry',
                        'storage_type'      => '$',
                        'save'              => 1,
                        'fortran_data_type' => 'character',
                    );
                    write_subroutine(
                        'if ($Entry) {($_, $Entry) = ($Entry, ""); goto $_;}');
                }
            }
        },

        'endfile' => sub {
            not_implemented();
        },

        'entry' => sub {
            if ( $program_unit_type !~ /^(subroutine|function)$/ ) {
                error(
"misplaced 'entry' statement; not allowed in code block type '$program_unit_type'\n"
                );
            }
            elsif ( $squeezed_text =~ /^entry(\w+)(\(.*\))?$/ ) {
                my $entry_name = $1;
                $entry_name = capitalization_check($entry_name);
                my $Entry_name = uc $entry_name;
                write_label("$Entry_name:");
                mark_target("$Entry_name:");
                my $rargs = scan_args($2);

                # we'll save the args and process when we see the 'END' stmt
                push @entry_points, [ $entry_name, $rargs ];
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'equivalence' => sub { },

        # nonstanard for f77
        'exit' => sub {
            unless ( $stmt_signature eq 'k' ) {
                unrecognized($stmt_type);
            }
            write_code("last;");
        },

        'external' => sub { },

        'format' => sub {

            my $varname = "\$fmt_$stmt_label";

            DEBUG_FORMAT && do {
                my $fmt = join_tokens( 1, @$rtokens - 1 );
                $fmt = quote_string($fmt);
                write_format("## $varname=$fmt;");
            };

            # use the raw input line to avoid quoting problems
            my $raw = $input_line;
            $raw =~ s/^([^\(])+//;
            my ( $fmt, $count ) =
              Fortran::F77toPerl::Format::convert_to_perl($raw);
            write_format("my $varname=$fmt;");
        },

        'function' => sub {

            my $i;
            for ( $i = 0 ; $i <= @$rtokens - 1 ; $i++ ) {
                last if ( $rtokens->[$i] eq 'function' );
            }
            $squeezed_text = join_tokens( $i, @$rtokens - 1 );
            if ( $squeezed_text =~ /^function(\w+)(\(.*\))?$/ ) {
                my $lc_name = $1;
                $program_unit_name = capitalization_check($lc_name);
                $program_unit_type = 'function';
                write_subroutine("sub $program_unit_name {");
                write_outer_block_name("sub $program_unit_name");
                @main_args = scan_args($2);
                if ( $non_comment_count != 1 ) {
                    error("function statement out of order\n");
                }
                my $fortran_data_type = 'real';
                if ( $i > 0 ) {
                    $fortran_data_type = $rtokens->[0];
                }
                $local_symbol_table->set(
                    'name'              => $lc_name,
                    'storage_type'      => '$',
                    'fortran_data_type' => $fortran_data_type,
                );
            }
            else {
                unrecognized('function');
            }
        },

        'goto' => sub {

            # unconditional goto: like "goto 500"
            if ( $squeezed_text =~ /^goto(\d+)$/ ) {
                my $target = $1;
                my $label  = get_goto_label($target);
                write_code("goto $label;");
            }

            # assigned goto: ignore the list of labels
            #  go to $igo,(120,150,180,210)
            elsif ( $squeezed_text =~ /^goto(\$\w+)(,.*)?$/ ) {
                write_code("goto $1;");
            }

            # computed goto; this will be translated as:
            #   F77:   goto (10,20,30,40), i
            #   perl:  goto qw(L10 L20 L30 L40)[$i-1];
            elsif ( $squeezed_text =~ /^goto\((\d+(,\d+)*)\),?(.*)$/ ) {
                my $ilabels = $1;
                my $var     = $3;
                my @ilabels = split ',', $ilabels;
                my $olabels = "";
                foreach (@ilabels) {
                    my $olabel = get_goto_label($_);
                    $olabels .= " " . $olabel;
                }
                my $count = @ilabels;
                write_code(
                    "goto qw($olabels )[$var-1] if ($var >0 && $var <= $count);"
                );
            }
            else {
                unrecognized('goto');
            }
        },

        'if' => sub {
            if ( $squeezed_text =~ /^if(\(.+\))([^\)]+)$/ ) {
                my $expr     = $1;
                my $the_rest = $2;

                # logical if block:  if (...) then
                # Note: all logical if's of the form
                #    if (expression) statement
                # have already been converted into logical if blocks
                if ( $the_rest eq 'then' ) {
                    write_code("if $expr {");
                }

                # arithmetic if
                elsif ( $the_rest =~ /^(\d+),(\d+),(\d+)$/ ) {
                    my $label_1 = $1;
                    my $label_2 = $2;
                    my $label_3 = $3;
                    $label_1 = get_goto_label($label_1);
                    $label_2 = get_goto_label($label_2);
                    $label_3 = get_goto_label($label_3);
                    $expr =~ s/\)$//;
                    write_code("if $expr < 0) {goto $label_1}");
                    write_code("elsif $expr == 0) {goto $label_2}");
                    write_code("else {goto $label_3}");
                }
                else {
                    unrecognized('if');
                }
            }
        },

        'implicit' => sub {
            if ( $squeezed_text =~ /^implicit([^\(]+)\(([^\)]+)\)$/ ) {
                my $type = $1;
                my @terms = split ',', $2;
                foreach my $term (@terms) {
                    letter_range( $type, $term );
                }
            }
            elsif ( $squeezed_text =~ /^implicitnone$/ ) {
                %implicit_fortran_type = ();
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'inquire' => sub {

            if ( $squeezed_text =~ /^inquire\(.*\)$/ ) {
                my $max_index    = @$rtokens - 1;
                my $i_open_paren = 1;
                my @pairs        =
                  scan_control_list( $i_open_paren + 1, $max_index - 1,
                    'unit' );
                my $call_hash = "F77_inquire(";
                foreach my $term (@pairs) {
                    my ( $name, $value, $iv_beg, $iv_end ) = @$term;
                    $call_hash .= "$name => $value,";
                }
                $call_hash .= ");";
                write_code($call_hash);
            }
            else {
                unrecognized($stmt_type);
            }
        },

        # ignore intrinsic statement for now
        'intrinsic' => sub {
        },

        'namelist' => sub {
            not_implemented();
        },

        'open' => sub {

            if ( $squeezed_text =~ /^open\(.*\)$/ ) {

                my $max_index    = @$rtokens - 1;
                my $i_open_paren = 1;
                my @pairs        =
                  scan_control_list( $i_open_paren + 1, $max_index - 1,
                    'unit' );
                $need_IOlib = 1;
                my $call_hash = "\$IOlib->open(";
                my ($err);
                foreach my $term (@pairs) {
                    my ( $name, $value, $iv_beg, $iv_end ) = @$term;
                    if ( $name eq 'iostat' ) { $value = '\\' . $value }
                    elsif ( $name eq 'err' ) {
                        $err   = $value;
                        $value = '\$ERR';
                        make_io_return_variables('ERR');
                    }
                    $call_hash .= "$name => $value,";
                }
                $call_hash .= ");";
                write_code($call_hash);
                write_io_return_goto_code( 'ERR', $err ) if $err;
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'pause' => sub {
            my $msg = "";
            if ( $stmt_signature eq 'k' ) {
            }
            elsif ( $stmt_signature =~ /^k.$/ ) {
                $msg = $rtokens->[1];
                my $type = $rtoken_types->[1];

                # could write error if type is now Q or d
                # but will just quote whatever is after keyword stop
                if ( $type ne 'Q' ) { $msg = "'" . $msg . "'" }
            }
            else {
                unrecognized('pause');
            }
            $saw_builtin{'PAUSE'}++; 
            write_code("PAUSE($msg);");
        },

        'parameter' => sub { },

        'print' => sub {
            make_io_code();
        },

        'program' => sub {

            if ( $squeezed_text =~ /^program(\w+)(\(.*\))?$/ ) {
                $program_unit_name = capitalization_check($1);
                $main_program      = $program_unit_name;
                $program_unit_type = 'program';
                my $line = "sub $program_unit_name {";
                write_code("$line");
                write_outer_block_name("sub $program_unit_name");

                if ($2) {
                    write_comment("## $2=\@_;");
                }
                if ( $non_comment_count != 1 ) {
                    error("program statement out of order\n");
                }
            }
            else {
                unrecognized('program');
            }
        },

        'read' => sub {
            make_io_code();
        },

        'return' => sub {
            if ( $program_unit_type !~ /^(program|subroutine|function)$/ ) {
                error("return not valid here\n");
            }

            # simple return
            if ( $stmt_signature eq 'k' ) {

                # if the next non-comment line of code is
                # the 'end' statement, we can skip the next 2 lines
                my $next = $next_statement_text->();
                if ( $next !~ /^\s*END$/i ) {
                    write_code("goto RETURN;");
                    mark_target("RETURN:");
                }
            }

            # alternate return
            else {
                unless ($has_alternate_return) {
                    warning("no alternate return labels seen in arg list\n");
                }
                $squeezed_text =~ s/^return/return /;
                write_code( $squeezed_text . ';' );
            }
        },

        'rewind' => sub {
            shift @$rtokens;
            my $lun = join '', @$rtokens;
            if ($lun) {
                write_code("\$IOlib->FH($lun)->seek(0,0);");
            }
            else {
                unrecognized($stmt_type);
            }
        },

        'save' => sub { },

        'stop' => sub {
            if ( $stmt_signature eq 'k' ) {

            }
            elsif ( $stmt_signature =~ /^k.$/ ) {
                my $tok  = $rtokens->[1];
                my $type = $rtoken_types->[1];

                # could write error if type is now Q or d
                # but will just quote whatever is after keyword stop
                # NOTE: g77 seems to write stop messages to stderr
                $tok =~ s/"/\\"/g;
                write_code("print STDERR \" STOP  $tok statement executed\\n\";");
            }
            else {
                unrecognized('stop');
            }
            write_code("exit;");
        },

        'subroutine' => sub {
            if ( $squeezed_text =~ /^subroutine(\w+)(\(.*\))?$/ ) {
                $program_unit_name = capitalization_check($1);
                $program_unit_type = 'subroutine';
                write_subroutine("sub $program_unit_name {");
                write_outer_block_name("sub $program_unit_name");
                @main_args = scan_args($2);
                if ( $non_comment_count != 1 ) {
                    error("subroutine statement out of order\n");
                }
            }
            else {
                unrecognized('subroutine');
            }
        },

        'write' => sub {
            make_io_code();
        },
    };

    sub convert_program_unit {

        # given:
        #   $rsubroutine   = reference to list of lines of fortran code
        #   $rlabel_index = reference to hash table of label number locations
        #
        # returns:
        #   $routput  = reference to list of lines of perl code
        #               (which still needs to be tidied)

        ( $program_unit_count, my $rsubroutine, $rlabel_index, my $debugger ) =
          @_;

        new_program_unit();
        my @more_text;
        my $line_index;

        # sub which may be called
        $next_statement_text = sub {

            if (@more_text) { return $more_text[$#more_text] }
            foreach my $i ( $line_index + 1 .. @$rsubroutine - 1 ) {
                my (
                    $next_input_line, $next_input_line_number,
                    $next_filename,   $next_stmt_label,
                    $next_stmt_text
                  )
                  = @{ $rsubroutine->[$i] };

                # skip comments
                if ( $next_stmt_text =~ /^#/ || $next_stmt_text =~ /^\s*$/ ) {
                    next;
                }
                return $next_stmt_text;
            }
            return "";
        };

        # loop over all statements in this subroutine
        $line_index = -1;
        foreach ( 0 .. @$rsubroutine - 1 ) {
            $line_index++;
            (
                $input_line, $input_line_number, $filename, $stmt_label,
                $stmt_text
              )
              = @{ $rsubroutine->[$line_index] };

            $stmt_type = "";

            # just write out comments
            if ( $stmt_text =~ /^#/ || $stmt_text =~ /^\s*$/ ) {
                write_comment("$stmt_text");
                next;
            }

            # skip an isolated semicolon to avoid parsing errors
            next if ( $stmt_text =~ /^[;\s*]+$/ );

            # always create a label for numbered statements
            # (unused labels will be removed later)
            if ($stmt_label) { write_label("L$stmt_label:") }

            # it's code of some type...
            push @more_text, $stmt_text;
            while ( my $input_stmt = pop @more_text ) {
                my ( $rline_of_tokens, $rmore_text ) =
                  Fortran::F77toPerl::Lex77::tokenize_fortran_statement(
                    $input_stmt, $input_line_number );
                $debugger->write_debug_entry($rline_of_tokens) if $debugger;
                if ($rmore_text) { push @more_text, @$rmore_text }

                $rtoken_types = $rline_of_tokens->{_rtoken_types};
                $rtokens      = $rline_of_tokens->{_rtokens};
                $stmt_type    = $rline_of_tokens->{_line_type};
                my $max_token_index = @$rtokens - 1;

                my $lex_error = $rline_of_tokens->{_lex_error};
                if ($lex_error) {
                    $bad_line_count++;
                    if ( $bad_line_count > 30 ) {
                        print STDERR "Too many errors ... Exiting\n";
                        exit(1);
                    }
                }
                my $side_comment = $rline_of_tokens->{_side_comment};

                if ($lex_error) {
                    write_comment("## $input_stmt");
                    next;
                }

                if ($side_comment) {
                    write_side_comment($side_comment);
                }

                # count lines of non-comments to allow checking
                # subroutine, function, blockdata, and program statements
                $non_comment_count++;

                check_statement_order();

                # translate some tokens into perl
                translate_tokens_to_perl();

                # form squeezed_text of line for simple parsing opportunities
                $squeezed_text = join '', @$rtokens;

                # the signature is the join of token types, and is sometimes
                # used for quick tests
                $stmt_signature = join '', @$rtoken_types;

                # FIXME: need special check for first line, such
                # as integer function xxx(

                # check for a handler for this statement type
                if ($stmt_type) {
                    my $code = $tokenization_code->{$stmt_type};
                    if ($code) {
                        $code->();
                        next;
                    }
                    else {
                        unrecognized($stmt_type);
                        next;
                    }
                }

                my $perl_text = $squeezed_text;

                # add terminal semicolon
                if ( $perl_text !~ /^[\{\}]$/ ) { $perl_text .= ';' }

                # output the code
                write_code("$perl_text");
            }
        }
        continue {

            # append any associated closing braces created for
            # this statement
            if ( $closing_braces[$line_index] ) {
                write_code("$closing_braces[$line_index]");
            }
        }

        # create all declarations for this code block
        make_declarations();

        # we're done with this function ... get and return the code
        return get_perl_function();
    }

    my %is_declaration;
    my %is_data_type;
    my %is_executable;

    ## FIXME: may be able to remove complexdouble
    BEGIN {
        @_ = qw{
          byte
          character
          common
          complexdouble
          complex
          dimension
          doublecomplex
          doubleprecision
          external
          integer
          intrinsic
          logical
          real
          save
        };
        @is_declaration{@_} = (1) x scalar(@_);

        @_ = qw{
          byte
          character
          complexdouble
          complex
          doubleprecision
          doublecomplex
          integer
          logical
          real
        };
        @is_data_type{@_} = (1) x scalar(@_);

        # This is probably still incomplete...
        # Note: 'end' is included as an executable statement
        # to trigger resolution of equivalences by at least the
        # last statement
        @_ = qw{
          assign
          backspace
          blockdata
          call
          close
          continue
          do
          else
          elseif
          enddo
          endfile
          endif
          end
          goto
          if
          inquire
          open
          pause
          print
          read
          return
          rewind
          stop
          write
          encode
          decode
          accept
        };
        @is_executable{@_} = (1) x scalar(@_);

        # non-standard for f77:
        @_ = qw{
          cycle
          dowhile
          exit
        };
        @is_executable{@_} = (1) x scalar(@_);
    }

    sub check_statement_order {

        # %saw_statement tracks progress through a file and can have
        # these values:
        #
        # 'subroutine'  (subroutine, function, program, blockdata)
        # 'implicit'
        # 'equivalence'
        # 'declaration' (common, dimension, external,
        #    intrinsic, save, complex, real, doubleprecision, integer,
        #    character, logical)
        # 'data'
        # 'executable'
        # 'statementfunction'
        # 'parameter'

        # patch to handle 'type' and 'accept' DEC extensions
        # by translating them to print and read
        if ( $stmt_type eq 'type' ) {
            $stmt_type = 'print';
            $rtokens->[0] = $stmt_type;
        }
        elsif ( $stmt_type eq 'accept' ) {
            $stmt_type = 'read';
            $rtokens->[0] = $stmt_type;
        }

        my $old_saw_ex = $saw_statement_type{executable};
        if ( $stmt_type =~ /^(subroutine|function|program|blockdata)$/ ) {
            if ( $non_comment_count != 1 ) {
                error(
"a $stmt_type statement must be the first non-comment line\n"
                );
            }
            else {
                $saw_statement_type{subroutine} = 1;
            }
        }

        elsif ( $stmt_type eq 'implicit' ) {
            $saw_statement_type{implicit} = 1;
        }

        elsif ( $stmt_type eq 'data' ) {
            $saw_statement_type{data} = 1;
        }
        elsif ( $stmt_type eq 'parameter' ) {
            $saw_statement_type{parameter} = 1;
        }
        elsif ( $stmt_type eq 'equivalence' ) {
            $saw_statement_type{equivalence} = 1;
        }
        elsif ( $is_declaration{$stmt_type} ) {

            my $is_function;
            if ( $is_data_type{$stmt_type} && $non_comment_count == 1 ) {

                # FIXME: should go through tokens instead
                my $squeezed_text = join '', @$rtokens;
                if ( $squeezed_text =~ /$stmt_type(.*)function/ ) {
                    if ( $1 && $1 !~ /^[\d\*\(\)]+$/ ) {
                        print STDERR
                          "Program Error parsing function type at $1\n";
                    }
                    $is_function = 1;
                    $saw_statement_type{subroutine} = 1;
                }
            }

            unless ($is_function) {
                $saw_statement_type{declaration} = 1;
                if ( $saw_statement_type{executable} == 2 ) {
                    error(
                        "a $stmt_type must not follow an executable statement\n"
                    );
                }
            }
        }

        elsif ( $is_executable{$stmt_type} ) {
            $saw_statement_type{executable} = 2;
        }

        # no checks for these
        elsif ( $stmt_type =~ /^(entry|format)$/ ) {
        }

        # Catch any missed types
        elsif ($stmt_type) {
            print STDERR
              "Program error..missing entry for statement type: $stmt_type\n";
        }

        else {

            unless ( $saw_statement_type{executable}
                && $saw_statement_type{executable} == 2 )
            {

                # We have to decide if this is a statement function or
                # executable statement.
                # Unlike the other flags, this one has 3 values:
                # $saw_statement_type{executable} =
                #      = 0 if haven't seen executable
                #      = 1 if we are in statement function definitions
                #      = 2 if we are in actual executable code
                $stmt_signature = join '', @$rtoken_types;
                if ( $stmt_signature =~ /^w\(.*\)=.*$/ ) {

                    my $word = $rtokens->[0];
                    my $type =
                      $local_symbol_table->get( $word, 'storage_type' );
                    if ( $type && $type eq '@' ) {
                        $saw_statement_type{executable} = 2;
                    }
                    elsif ( $type && $type eq '$' ) {
                        my $fortran_data_type =
                          $local_symbol_table->get( $word,
                            'fortran_data_type' );
                        if ( $fortran_data_type eq 'character' ) {
                            $saw_statement_type{executable} = 1;
                            my $i_opening_paren = 1;
                            my ( $i_colon, $colon ) =
                              find_next_comma( $i_opening_paren + 1, ':' );
                            if ( $colon eq ':' ) {
                                $saw_statement_type{executable} = 2;
                            }
                            else {
                                $saw_statement_type{executable} = 1;
                            }
                        }
                        else {
                            $saw_statement_type{executable} = 1;
                        }
                    }
                    else {
                        $saw_statement_type{executable} = 1;
                    }
                }
                else {
                    $saw_statement_type{executable} = 2;
                }
            }
        }

        if (   $non_comment_count == 1
            && $program_unit_count > 1
            && !$saw_statement_type{subroutine} )
        {
            error("Expecting subroutine, function, program, or blockdata\n");
        }

        if (
            !$froze_storage
            && (

                # we have to lay out commons before processing a data statement
                # if this is a blockdata section
                (
                       $program_unit_type eq 'blockdata'
                    && $saw_statement_type{data}
                )

                # we always have to lay out common blocks before processing an
                # executable statement
                || $saw_statement_type{executable} == 2
            )
          )
        {
            $froze_storage = 1;
            layout_common_blocks();
            resolve_equivalences();
        }

        if ( $old_saw_ex < 2 && $saw_statement_type{executable} == 2 ) {
            mark_first_executable_statement();
        }
    }

    sub make_index_string {

        # Given a reference to an array of dimensions,
        # Make a string which can be used in a sub to evaluate a fortran
        # array index
        #
        # Example:
        #    dimension a($imax,$jmax,$kmax,$lmax)
        # should produce the index string
        #    $_[0]-1+$imax * ($_[1]-1+$jmax * ($_[2]-1+$kmax * ($_[3]-1)))
        #
        my ( $rdims, $rimins ) = @_;
        my $n = @$rdims - 1;
        my $index_string;

        my $imin = $rimins->[$n];
        my $sign = '-';
        if ( $imin =~ s/^\-// ) { $sign = '+'; }
        elsif ( $imin eq '0' ) { $imin = ""; $sign = ""; }
        $index_string = "\$_[$n]" . "$sign" . "$imin";
        for ( my $i = $n - 1 ; $i >= 0 ; $i-- ) {
            my $d    = $rdims->[$i];
            my $imin = $rimins->[$i];
            my $sign = '-';
            if ( $imin =~ s/^\-// ) { $sign = '+'; }
            elsif ( $imin eq '0' ) { $imin = ""; $sign = ""; }
            $index_string =
              "\$_[$i]" . "$sign" . "$imin + $d * (" . $index_string . ")";
        }
        return $index_string;
    }

    sub get_index_string {
        my ($name) = @_;
        $local_symbol_table->get( $name, 'index_string' );
    }

    sub make_call_arg_declarations {
        my ($is_entry) = shift;
        return unless @_;
        my @names = @_;

        # we can use 'my' vars in many places if there are no entry points
        my $local = scalar @entry_points ? 'local' : 'my';

        my $write_arg_code = sub {
            my ($line) = @_;
            if ($is_entry) {
                write_code($line);
            }
            else {
                write_args($line);
            }
        };

        my $write_use_vars_list = sub {

            # we have to create 'use vars' lines to keep strict happy
            my ($prefix) = shift;
            my @list;
            if ( !$is_entry ) { @list = @_ }
            else {
                @list = grep { !$is_main_arg{$_} && !$saw_entry_arg{$_} } @_;
            }

            # Note: this qw can be too long for one line and will not
            # be reformatted; may need to break it into pieces
            if (@list) {
                $_ = "($prefix" . ( join " $prefix", @_ ) . ')';
                $_ = "use vars qw$_;\n";
                if ($is_entry) {
                    write_use_vars($_);
                }
                else {
                    write_args($_);
                }
            }
        };

        my @arg_scalar;
        my @arg_array;
        my @dummy_args;

        # always set $R_xxx variables equal to the call args
        if ( $local eq 'local' ) {
            $write_use_vars_list->( '$R_', @names );
        }
        $write_arg_code->(
            "$local (\$R_" . ( join ', $R_', @names ) . ') = @_;' );

        # separate the args into scalars and arrays
        foreach my $name (@names) {

            my $storage_type = get_storage_type($name);
            if ($storage_type) {
                if ( $storage_type eq '@' ) {
                    push @arg_array, $name;
                }
                elsif ( $storage_type eq '$' ) {
                    push @arg_scalar, $name;
                }
                else {

                    # nothing more to declare for externals
                    ##error("DEBUG: arg $name has type $storage_type\n");
                }
            }
            else {
                push @dummy_args, $name;
            }
        }

        # handle the scalar call args
        if (@arg_scalar) {

            $write_use_vars_list->( '$', @arg_scalar );
            $_ = '($R_' . join( ', $R_', @arg_scalar ) . ');';
            $write_arg_code->(
                'local (*' . ( join ', *', @arg_scalar ) . ") = $_" );
        }

        # handle the array call args
        if (@arg_array) {

            foreach my $arg (@arg_array) {
                if (   $local_symbol_table->get( $arg, 'need_access_sub' )
                    || $local_symbol_table->get( $arg, 'need_call_sub' ) )
                {

                    # FIXME: would be nice to combine these two lines:
                    if ( $local eq 'local' ) {
                        $write_use_vars_list->( '$ARRAY_REF_', $arg );
                        $write_use_vars_list->( '$OFFSET_',    $arg );
                    }
                    $write_arg_code->(
"$local (\$ARRAY_REF_$arg, \$OFFSET_$arg) = \@{\$R_$arg};"
                    );
                }
            }

           # example:
           # my $xpoly=sub : lvalue {$ARRAY_REF_xpoly->[$OFFSET_xpoly+$_[0]-1]};
            $write_arg_code->(" ");
            if ( $local eq 'local' ) {
                $write_use_vars_list->( '$', @arg_array );
            }
            foreach my $arg (@arg_array) {
                if ( $local_symbol_table->get( $arg, 'need_access_sub' ) ) {
                    my $index_string = get_index_string($arg);
                    $write_arg_code->(
                            "$local \$$arg=sub : lvalue {\$ARRAY_REF_$arg"
                          . "->[\$OFFSET_$arg+$index_string]};" );
                }
            }

            $write_arg_code->(" ");
            $write_use_vars_list->( '$C_', @arg_array );
            foreach my $arg (@arg_array) {
                if ( $local_symbol_table->get( $arg, 'need_call_sub' ) ) {
                    my $index_string = get_index_string($arg);
                    $write_arg_code->(
"$local \$C_$arg=sub {[\$ARRAY_REF_$arg, \$OFFSET_$arg+$index_string]};"
                    );
                }
            }
        }

        # make note of any dummy call args
        if (@dummy_args) {
            $_ = "    ## DUMMY ARGS: " . join( ",", @dummy_args );
            $write_arg_code->($_);
        }

        # remember which entry args we've seen to avoid multiple
        # 'use vars' declarations
        if ($is_entry) {
            foreach my $name (@names) {
                $saw_entry_arg{$name} = 1;
            }
        }
    }

    sub find_best_offset {

        # find the the best symbolic offset for a variable by refering to
        # the master copy of a common block
        my ( $name, $common_name ) = @_;
        my $offset = $local_symbol_table->get( $name, 'common_offset' );

        my $best_offset = "";
        my $best_name   = "";
        foreach my $master_name (
            @{ $common_common_variable_name_order{$common_name} } )
        {
            my $master_offset =
              $common_symbol_tables{$common_name}
              ->get( $master_name, 'common_offset' );
            last if ( $master_offset > $offset );
            $best_name   = $master_name;
            $best_offset = $master_offset;
            last if ( $best_offset == $offset );
        }

        ## TODO: we should go through master equivalences here now
        ## if we still haven't found a zero difference

        ## TODO: try to make the local equivalence look like the
        ## equivalence if we haven't found a zero difference
        my $rname = $local_symbol_table->get( $name, 'equivalence_to' );
        if ($rname) {

        }

        if ($best_name) {
            my $diff = $offset - $best_offset;
            $offset = "\$$common_name" . "::" . "Offset{$best_name}";
            if ( $diff > 0 ) {
                $offset .= " + $diff";
            }
        }

        return $offset;
    }

    sub make_common_declarations {

        # loop over all common blocks in this code unit
        foreach my $common_name ( sort keys %rcommon_variables ) {

            # Save first occurance as the master copy
            unless ( $all_seen_common_names{$common_name} ) {
                $all_seen_common_names{$common_name} = 1;
                $common_symbol_tables{$common_name}  = $local_symbol_table;
                %{ $common_parameter_nonsymbolic_value{$common_name} } =
                  %parameter_nonsymbolic_value;
                %{ $common_parameter_symbolic_value{$common_name} } =
                  %parameter_symbolic_value;
                @{ $common_parameter_name_order{$common_name} } =
                  @parameter_name_order;
                %{ $common_parameter_dependents{$common_name} } =
                  %parameter_dependents;
                $common_common_variable_name_order{$common_name} =
                  $rcommon_variables{$common_name};
            }

            # Otherwise make local access subs for variables not in
            # the master copy.
            else {

                my @local_common_names;
                my @needless_equivalances;
                foreach
                  my $name ( @{ $rlocal_common_equivalences{$common_name} } )
                {
                    my $need_access_sub =
                      $local_symbol_table->get( $name, 'need_access_sub' );
                    my $need_call_sub =
                      $local_symbol_table->get( $name, 'need_call_sub' );
                    if ( $need_access_sub || $need_call_sub ) {
                        push @local_common_names, $name;
                    }
                    else {
                        push @needless_equivalances, $name;
                    }
                }

                next unless ( @local_common_names || @needless_equivalances );
                write_common("\n");
                write_common("# local equivalances to common /$common_name/");

                if (@needless_equivalances) {
                    write_common( "# Unused local equivalances: "
                          . ( join ", ", @needless_equivalances ) );
                }

                next unless (@local_common_names);

                # Make an access array for any common variables
                # not in the master common
                foreach my $name (@local_common_names) {

                    write_common("my (\$$name, \$C_$name);");
                    my $offset = find_best_offset( $name, $common_name );
                    my $storage_type =
                      $local_symbol_table->get( $name, 'storage_type' );
                    if ( $storage_type eq '@' ) {
                        my $index_string = get_index_string($name);
                        ## TESTING $offset .= " + $index_string";
                        $offset = "\@_ > 0 ? $offset + $index_string : $offset";
                    }
                    write_data(
                        "\$$name=sub : lvalue {\$$common_name" . "::" . "COMMON"
                          . "->[$offset]};" );
                    write_data( "\$C_$name=sub {[\$$common_name" . "::"
                          . "COMMON, $offset]};" );
                }
            }
        }
    }

    sub make_parameter_declarations {
        if (@parameter_name_order) {
            write_parameter_declaration("");
            write_parameter_declaration("# parameters");
            my $line = 'my ( $' . join( ', $', @parameter_name_order ) . ');';
            write_parameter_declaration($line);
        }
    }

    sub make_local_variable_declarations {
        my @symbols = ( $local_symbol_table->get_symbols() );
        my @saved_local_scalars;
        my @unsaved_local_scalars;
        my @saved_local_arrays;
        my @unsaved_local_arrays;
        my @local_equivalences;
        my @equivalenced_arrays;
        my @equivalenced_scalars;
        my %is_equivalenced;

        foreach my $name (@symbols) {

            # call args and common variables have already been done.
            next if $is_arg{$name};
            next if $is_parameter{$name};
            next if $local_symbol_table->get( $name, 'is_common' );

            my $storage_type = get_storage_type($name);
            if ( $storage_type eq '@' ) {
                if ( $local_symbol_table->get( $name, 'local_equivalence_name' )
                  )
                {
                    push @equivalenced_arrays, $name;
                    $is_equivalenced{$name} =
                      $local_symbol_table->get( $name,
                        'local_equivalence_name' );
                }
                elsif ( $local_symbol_table->get( $name, 'save' ) ) {
                    push @saved_local_arrays, $name;
                }
                else {
                    push @unsaved_local_arrays, $name;
                }
            }
            elsif ( $storage_type eq '$' ) {
                if ( $local_symbol_table->get( $name, 'local_equivalence_name' )
                  )
                {
                    push @equivalenced_scalars, $name;
                }
                elsif ( $local_symbol_table->get( $name, 'save' ) ) {
                    push @saved_local_scalars, '$' . $name;
                }
                else {
                    push @unsaved_local_scalars, '$' . $name;
                }
            }
            else {

                # x $x vx
            }
        }

        if (@saved_local_scalars) {
            write_save("");
            write_save("# saved scalars");
            my $line = "my ( " . ( join ',', @saved_local_scalars ) . ");";
            write_save($line);
        }

        if (@unsaved_local_scalars) {
            write_save("");
            write_save("# unsaved scalars");
            my $line = "my ( " . ( join ',', @unsaved_local_scalars ) . ");";
            write_declaration($line);
        }

        # routine to write the declarations
        my $write_array_declarations = sub {

            my ( $save_type, $rlocal_arrays ) = @_;
            my ( @call_sub, @access_sub );
            foreach my $name (@$rlocal_arrays) {

                if ( $local_symbol_table->get( $name, 'need_access_sub' ) ) {
                    push @access_sub, $name;
                }

                if ( $local_symbol_table->get( $name, 'need_call_sub' ) ) {
                    push @call_sub, $name;
                }
            }

            write_output_line( $save_type, " " );
            if ( $save_type eq 'save' ) {
                write_output_line( $save_type, "# saved arrays" );
            }
            else {
                write_output_line( $save_type, "# unsaved arrays" );
            }

            # declare storage for all (non-equivalenced) local arrays
            my $line;
            @_ = grep { !$is_equivalenced{$_} } @$rlocal_arrays;
            $line = 'my ( @' . ( join ', @', @_ ) . ');';
            write_output_line( $save_type, $line );

            if (@access_sub) {
                $line = 'my ( $' . ( join ', $', @access_sub ) . ');';
                write_output_line( $save_type, $line );
            }

            if (@call_sub) {
                $line = 'my ( $C_' . ( join ', $C_', @call_sub ) . ');';
                write_output_line( $save_type, $line );
            }

            if ( @access_sub || @call_sub ) {
                write_output_line( $save_type, "BEGIN {" );
                foreach my $name (@access_sub) {
                    my $index_string = get_index_string($name);
                    my $base_name    = $name;
                    if ( $is_equivalenced{$name} ) {
                        $base_name = $is_equivalenced{$name};
                    }
                    write_output_line( $save_type,
                            "\$$name=sub : lvalue {\$$base_name"
                          . "[$index_string]};" );
                }

                foreach my $name (@call_sub) {
                    my $index_string = get_index_string($name);
                    my $base_name    = $name;
                    if ( $is_equivalenced{$name} ) {
                        $base_name = $is_equivalenced{$name};
                    }
                    write_output_line( $save_type,
                        "\$C_$name=sub {[\\\@$base_name, $index_string]};" );
                }

                write_output_line( $save_type, " }" );
            }
        };

        @saved_local_arrays = ( @saved_local_arrays, @equivalenced_arrays );

        if (@saved_local_arrays) {
            $write_array_declarations->( 'save', \@saved_local_arrays );
        }

        if (@unsaved_local_arrays) {
            $write_array_declarations->( 'declaration',
                \@unsaved_local_arrays );
        }

        if (@equivalenced_scalars) {

            write_save("\n");
            write_save("# equivalenced scalars");
            foreach my $name (@equivalenced_scalars) {
                my $base_name =
                  $local_symbol_table->get( $name, 'local_equivalence_name' );
                my $storage_type      = get_storage_type($name);
                my $base_storage_type = get_storage_type($base_name);

                # Handle scalar->scalar
                if ( $base_storage_type eq '$' ) {
                    write_save( "my \$$name=" . '\$' . "$base_name;" );
                }

                # Handle scalar->array
                elsif ( $base_storage_type eq '@' ) {
                    my $offset =
                      $local_symbol_table->get( $name,
                        'local_equivalence_offset' );
                    write_save(
                        "my \$$name=" . '\$' . "$base_name" . "[$offset];" );
                }
            }
        }
    }

    sub make_declarations {
        make_call_arg_declarations( 0, @main_args );
        make_common_declarations();
        make_parameter_declarations();
        make_local_variable_declarations();
    }

    my %do_not_scan_for_barewords;
    my %builtin;

    BEGIN {

        # Do not scan these
        @_ = qw{
          blockdata
          end
          entry
          format
          function
          implicit
          intrinsic
          program
          subroutine
        };
        @do_not_scan_for_barewords{@_} = (1) x scalar(@_);

        #    fortran => perl or sub call
        %builtin = (
            'abs'    => 'abs',
            'aint'   => 'int',
            'alog'   => 'log',
            'alog10' => 'log10',
            'amax1'  => 'max',
            'amax1'  => 'max',
            'amin1'  => 'min',
            'and'    => 'iand',
            'atan'   => 'atan2',
            'atan2'  => 'atan2',
            'char'   => 'chr',
            'cos'    => 'cos',
            'dabs'   => 'abs',
            'datan'  => 'atan2',
            'datan2' => 'atan2',
            'dble'   => 'dble',
            'dcos'   => 'cos',
            'dexp'   => 'exp',
            'dmax1'  => 'max',
            'dmin1'  => 'min',
            'dsign'  => 'sign',
            'dsign'  => 'sign',
            'dsin'   => 'sin',
            'dsqrt'  => 'sqrt',
            'dtan'   => 'tan',
            'exp'    => 'exp',
            'iabs'   => 'abs',
            'iand'   => 'iand',
            'ichar'  => 'ord',
            'ifix'   => 'int',
            'index'  => 'index_F77',
            'int'    => 'int',
            'ior'    => 'ior',
            'len'    => 'length',
            'lgt'    => 'lgt_F77',
            'llt'    => 'llt_F77',
            'log'    => 'log',
            'log10'  => 'log10',
            'max'    => 'max',
            'max0'   => 'max',
            'min'    => 'min',
            'min0'   => 'min',
            'or'     => 'ior',
            'second' => 'second',
            'sign'   => 'sign',
            'sign'   => 'sign',
            'sin'    => 'sin',
            'sqrt'   => 'sqrt',
            'system' => 'system',
            'tan'    => 'tan',
        );
    }

    sub use_simple_access {

        # The caller needs to access an array element.  Decide if simple
        # (perl type) array access using [ ] can be used.  local arrays
        # with dimension 1 may use perl array access, others must use
        # sub calls.
        my ($name) = @_;
        my $use_simple_access = 1;
        if (   $is_arg{$name}
            || $local_symbol_table->get( $name, 'is_common' )
            || $local_symbol_table->get( $name, 'local_equivalence_name' ) )
        {
            $use_simple_access = 0;
        }
        else {
            my $rimaxs = $local_symbol_table->get( $name, 'imaxs' );
            if ( @$rimaxs != 1 ) {
                $use_simple_access = 0;
            }
        }
        unless ($use_simple_access) {
            $local_symbol_table->set(
                'name'            => $name,
                'need_access_sub' => 1
            );
        }
        return $use_simple_access;
    }

    my %character_compare;

    BEGIN {
        %character_compare = (
            '==' => ' eq ',
            '!=' => ' ne ',
        );
    }

    sub scan_barewords {

        # look at the bareword tokens and decide what they are
        # label bare storage types as:
        # 'x' = sub or function call
        # '$x' = statement function
        # 'vx' = function with pure call by value
        # '@' = array name
        # '$' = scalar name
        # label bare fortran types as:
        # ... one-line function?

        return if ( $stmt_type && $do_not_scan_for_barewords{$stmt_type} );
        my $token;
        my $type;
        my $last_token              = " ";
        my $last_token_type         = "b";
        my $last_fortran_data_type  = " ";
        my $next_token              = $rtokens->[0];
        my $depth                   = 0;
        my $saw_subcall             = 0;
        my $i_equals                = 0;
        my $saw_lv_int              = 0;
        my $saw_rv_float            = 0;
        my $colon_count             = 0;
        my $saw_statement_function  = 0;
        my $saw_dimensions          = 0;
        my $next_token_type         = $rtoken_types->[0];
        my @paren_type              = ('X');
        my @paren_fortran_data_type = (' ');
        my $bareword;
        my $storage_type;
        my $fortran_data_type = "";
        my $max_token_index   = @{$rtokens} - 1;
        my @changeme;
        my $i_start = 0;

        # tokens to be inserted will be stored in a hash and inserted at the
        # end of this routine
        my %insert_before = ();
        my $store_inserted_tokens = sub {
            my ( $i_insert_before, $rinsert_tokens, $rinsert_token_types ) = @_;

            # shouldn't happen..
            # in future coding, we can allow collisions by combining the
            # new list with the old list, but we need a rule to define the
            # order.
            if ( defined( $insert_before{$i_insert_before} ) ) {
                error(
                    "Programming error: unexpected collision; please check\n");
            }
            $insert_before{$i_insert_before} =
              [ $rinsert_tokens, $rinsert_token_types ];
        };

        # Look for and mark labeled common block
        if ( $stmt_type eq 'common' && $max_token_index >= 3 ) {
            if ( $rtoken_types->[1] eq '/' && $rtoken_types->[3] eq '/' ) {
                $rtoken_types->[2] = 'n';
            }
        }

        for my $i ( 0 .. $max_token_index ) {
            $token = $next_token;
            $type  = $next_token_type;

            if ( $i < $max_token_index ) {
                $next_token      = $rtokens->[ $i + 1 ];
                $next_token_type = $rtoken_types->[ $i + 1 ];
            }
            else {
                $next_token      = " ";
                $next_token_type = 'b';
            }
            if ( $type eq '=' && $depth == 0 ) {
                if ($i_equals) {
                    error(
                        "invalid assignment statement; only one '=' allowed\n");
                }
                $i_equals = $i;

            # no need to convert the rhs to int when the next token is a
            # function returning int
            # FIXME: add to this list as necessary
            # FIXME: we really don't know yet if the next word is a function!
            # We should be sure that it is followed by an opening paren
            # Better yet, do this in a separate loop. However, when we translate
            # something like iabs to abs, we lose important information.
                if ( $next_token =~ /^(int|mod|ichar|index|len|iabs)$/ ) {
                    $saw_lv_int = 0;
                }
                next;
            }
            if ( $type eq ')' ) {
                $fortran_data_type = $paren_fortran_data_type[$depth];
                $depth--;
                next;
            }
            elsif ( $type eq '(' ) {
                $depth++;
                $paren_type[$depth]              = $last_token_type;
                $paren_fortran_data_type[$depth] = $last_fortran_data_type;
                next;
            }

            # catch simple integer divides; flag others
            ## TODO
            elsif ( $type eq '/' ) {
            }

            # convert '==' and '!=' to 'eq' and 'ne' when appropriate
            elsif ( $type =~ /^[=!]=$/ ) {

#print "DEBUG: type is $type tok is $token last is $last_token_type next=$next_token_type last_storage=$last_fortran_data_type\n";
                if (   $last_token_type eq 'Q'
                    || $next_token_type        eq 'Q'
                    || $last_fortran_data_type eq 'character' )
                {
                    $token = $character_compare{$type};
                    $saw_builtin{'eq'}++;    # just count 'eq' and 'ne' together
                }
            }

            # -----------------------
            # Now classify a bareword
            # -----------------------
            next unless ( $type eq 'w' );

            $bareword = $token;

            # -----------------------------------------------------
            # Filter out non-variable words
            # -----------------------------------------------------
            # something like 'unit' and 'name' in the following must
            # not be labeled as variables:
            #    inquire(unit=nfile,name=fname)
            #
            # but we have to watch out in this one that 'err' is not
            # a variable but 'l' is:
            #    read (nfile,err=1000) (buffer(l),l=1,length)
            # We can use the fact that the paren type is 'k' for the
            # paren following keyword 'read'.
            if (   $next_token eq '='
                && $depth == 1
                && $paren_type[$depth] eq 'k'
                && $stmt_type =~ /^(inquire|open|close|read|write)$/ )
            {
                $type = 'k';
                next;
            }

            # lookup existing type of the word, if any
            $storage_type      = get_storage_type($token);
            $fortran_data_type =
              $local_symbol_table->get( $token, 'fortran_data_type' );

            if ( $is_data_type{$stmt_type} && $depth == 0 ) {
                $fortran_data_type = $stmt_type;
            }

            # use implicit rules to find type if necessary
            if ( !$fortran_data_type ) {
                my $char = lc( substr( $bareword, 0, 1 ) );
                $fortran_data_type = $implicit_fortran_type{$char};
                $used_implicit_fortran_type{$char} = 1;
                if ( !defined($fortran_data_type) ) { $fortran_data_type = "" }
            }

            # change parameters to upper case
            if (   $stmt_type eq 'parameter'
                && $next_token_type eq '='
                && $depth == 1 )
            {

                # Check and copy any symbol table entry for the lower
                # case word to the upper case word upper case word. For
                # example, we might have:
                #    character*8 myname
                #    parameter(myname='CRUNCHER')

                $bareword = uc($token);
                if ( $local_symbol_table->defined($token) ) {
                    if ( $local_symbol_table->get( $token, 'is_common' ) ) {
                        error(
"Parameter '$token' has been defined to be a common variable\n"
                        );
                    }
                    elsif ( $local_symbol_table->get( $token, 'is_arg' ) ) {
                        error(
"Parameter '$token' has been defined to be a common variable\n"
                        );
                    }
                    elsif ( $storage_type && $storage_type ne '$' ) {
                        error(
"Parameter '$token' has already been defined as type '$storage_type\n"
                        );
                    }

                    $local_symbol_table->copy( $token, $bareword );
                    $local_symbol_table->delete($token);
                }

                if ( $is_parameter{$bareword} ) {
                    error("Parameter '$bareword' has already been defined\n");
                }
                $is_parameter{$token} = 1;
                $token                = $bareword;
                $is_parameter{$token} = 1;
                $type                 = '$';
                next;
            }
            elsif ( $is_parameter{$token} ) {

                # Shouldn't happen if we intercepted tokens correctly
                if ( $local_symbol_table->defined($token) ) {
                    error(
"Program error: lower case symbol table entry shouldn't exist for parameter $token \n"
                    );
                }
                $bareword = $token = uc($token);
                if ( $next_token_type eq '=' ) {
                    error("parameter $token appears to left of '='\n");
                }
            }

            # -----------------------
            # Case 1a. subroutine call
            # -----------------------
            if ( $stmt_type eq 'call' && $i == 1 ) {
                $type = 'x';
            }

            # -----------------------
            # Case 1b. external declaration
            # -----------------------
            elsif ( $stmt_type eq 'external' && $i == 1 ) {
                $type = 'x';
            }

            # ---------------------------------------
            # Case 2. handle declaration or type info
            # ---------------------------------------
            elsif ( $is_declaration{$stmt_type} && $depth == 0 ) {

                if ( $storage_type && $storage_type ne '$' ) {
                    $type = $storage_type;
                }
                elsif ($saw_statement_type{executable}
                    && $saw_statement_type{executable} == 1 )
                {
                    $saw_statement_function = 1;
                    $type                   = '$x';
                }
                elsif ( $last_token eq 'function' ) {
                    $type = 'x';
                }
                elsif ( $next_token_type eq '(' ) {
                    $type = '@';
                }
                else {
                    $type = '$';
                }

                if ( $type eq '@' && $next_token_type eq '(' ) {
                    $saw_dimensions = 1;
                }
            }

            # ---------------------------------------
            # Case 2a. equivalence
            # ---------------------------------------
            elsif ( $stmt_type eq 'equivalence' && $depth == 1 ) {
                if ( $storage_type && $storage_type ne '$' ) {
                    $type = $storage_type;
                }
                elsif ( $next_token_type eq '(' ) {
                    $type = '@';
                }
                else {
                    $type = '$';
                }
            }

            # ----------------------------------------------
            # Case 3. handle a word followed by open paren..
            # ----------------------------------------------
            elsif ( $next_token_type eq '(' ) {

                # mark bareword followed by ( as function
                # or array unless character
                if ($storage_type) { $type = $storage_type }
                if ( $storage_type && $storage_type ne '$' ) {
                    $type = $storage_type;
                }
                else {

                    # it's a function call unless it has been declared
                    # a character and the parens contain a colon
                    my $is_character = 0;
                    if (   $fortran_data_type
                        && $fortran_data_type eq 'character' )
                    {
                        my $i_colon = find_next_comma( $i + 2, ':' );
                        if ( $rtoken_types->[$i_colon] eq ':' ) {
                            $is_character = 1;
                        }
                    }
                    ##print "DEBUG $token $type $storage_type is=$is_character\n";
                    unless ($is_character) {

                        # check for statement function definition
                        if (   $saw_statement_type{executable} == 1
                            && !$i_equals
                            && $depth == 0 )
                        {
                            $saw_statement_function = 1;
                            $type                   = '$x';
                        }
                        elsif ( $builtin{$token} ) {

                            ## FIXME: fix fortran_data_type of builtin if
                            # necessary

                            # do pre-conversions
                            if ( $token eq 'atan' || $token eq 'datan' ) {
                                my $i_closing = find_closing_paren( $i + 1 );
                                $store_inserted_tokens->(
                                    $i_closing,
                                    [ ',', '1' ],
                                    [ ',', 'd' ]
                                );
                            }

                            $type  = 'vx';
                            $token = $builtin{$token};
                            $saw_builtin{$token}++;
                        }
                        elsif ( $token eq 'mod' && $next_token eq '(' ) {
                            my $i_next_comma = find_next_comma( $i + 2 );
                            if ( $rtokens->[$i_next_comma] eq ',' ) {
                                $token = "";
                                $type  = 'b';

                                ## TODO: simplify when possible
                                $rtokens->[$i_next_comma]      = ')%(';
                                $rtoken_types->[$i_next_comma] = ')%(';
                            }
                            else {

                                # shouldn't happen
                                $type = 'x';
                            }
                        }
                        else {
                            $type = 'x';
                        }
                    }
                }

                # handle array..
                if ( $type eq '@' ) {

                    # if we are not in a sub call..
                    # (note: type '$x' anonymous sub calls use call by value so
                    # they can be processed here)
                    # CAUTION: The opposite of this statement must exist in
                    # process_call_args
                    unless ( $paren_type[$depth] eq 'x'
                        && $last_token_type =~ /^[\(\,]$/ )
                    {

                        # Handle a 1D local array
                        if ( use_simple_access($token) ) {

                            my $rimin_values =
                              $local_symbol_table->get( $bareword,
                                'imin_values' );
                            my $imin            = $rimin_values->[0];
                            my $i_closing_paren = find_closing_paren( $i + 1 );

                            # evaluate the index if numeric
                            if (   $i_closing_paren eq $i + 3
                                && $rtoken_types->[ $i + 2 ] eq 'd' )
                            {
                                $rtokens->[ $i + 2 ] -= $imin;
                            }

                            # otherwise appened tokens '-' and 'imin' if
                            # necessary
                            elsif ( $imin != 0 ) {
                                my $sign = '-';
                                if ( $imin < 0 ) { $sign = '+'; $imin = -$imin }
                                $store_inserted_tokens->(
                                    $i_closing_paren,
                                    [ $sign, $imin ],
                                    [ $sign, 'd' ]
                                );
                            }

                            push @changeme, [ $i + 1, '[' ];
                            push @changeme, [ $i_closing_paren, ']' ];
                            $token = '$' . $token;
                        }

                        # Handle non-local or multi-dimensional arrays
                        else {
                            $token = "\$$token->";
                            $token = rename_common_token($token);
                        }
                    }

               # otherwise, it is complex because of possible call by reference,
               # so sub process_call_args will fix things up
                    else {
                    }
                }

                # paren after a scalar should be character subscript
                elsif ( $type eq '$' ) {
                    unless ( $fortran_data_type eq 'character' ) {
                        error("Unexpected paren after '$token'\n");
                    }
                }
            }

            # -------------------------------------------
            # Case 4. handle a word not followed by a '('
            # -------------------------------------------
            else {

                # mark as scalar if not yet identified
                $type = $storage_type ? $storage_type : '$';

                if ( $paren_type[$depth] !~ /x$/ ) {

                    # try to catch unsubscripted arrays used as scalars but
                    # watch out for i/o operations on an entire array, like:
                    #    dimension i(10)
                    #    write (io, 113) i
                    if (   $type eq '@'
                        && $saw_statement_type{executable} == 2
                        && $stmt_type !~
                        /^(write|read|data|print|encode|decode)$/ )
                    {
                        error(
"Syntax error? $token is dimensioned but not subscritped here\n"
                        );
                    }

                    # warn about possible undefined scalars
                    # but filter out false warnings such as for dummy
                    # parameters like 'k4' here:
                    #   write(iou,131) (s1(k4),k4=1,4)
                    # and with 'ios' here:
                    #   close(unit=ipagef,iostat=ios,err=100,status='keep')
                    if (   $type eq '$'
                        && !$storage_type
                        && $next_token_type ne '='
                        && $stmt_type !~ /^(data|assign|write|read|close|open)$/
                        && $saw_statement_type{executable} == 2
                        && !$is_parameter{$token}
                        && !$is_arg{$token} )
                    {
                        warning("$token first seen here, may be undefined \n");
                    }
                }
            }
        }    # end of loop over all tokens in this statement

        continue {

            ##print "DEBUGT token=$token type=$type bare=$bareword \n";
            if ( $type eq '$' ) {
                $token = '$' . $token;
                if (
                    $local_symbol_table->get(
                        $bareword, 'local_equivalence_name'
                    )
                  )
                {
                    $token = '$' . $token;
                }
            }
            elsif ( $type eq 'x' ) {
                $saw_subcall = 1;
                $bareword    = capitalization_check($bareword);
                $token       = $bareword;
                if ( $is_arg{$bareword} ) {
                    $token = '$R_' . $token . '->';
                    ##error("DEBUG: calling external $token\n");
                }

                # check for things like 'call system',
                # t=second()
                if ( $builtin{$token} ) {
                    $type  = 'vx';
                    $token = $builtin{$token};
                    $saw_builtin{$token}++;
                }
            }
            elsif ( $type eq 'vx' ) {
                $saw_subcall = 1;
            }
            elsif ( $type eq '$x' ) {
                $saw_subcall = 1;
                $token       = "\$$token->";
            }

            # fix '==' and '!=' to be 'eq' and 'ne' if we haven't done so
            if (   $fortran_data_type eq 'character'
                && $last_token =~ /^[!=]=$/ )
            {
                ## FIXME: improve this message
                error(
"DEBUG: Converting $last_token at token=$token type=$fortran_data_type bare=$bareword type=$type \n"
                );
                $rtokens->[ $i - 1 ] = $character_compare{$last_token};
                $saw_builtin{'eq'}++;
            }

            $rtoken_types->[$i]     = $type;
            $rtokens->[$i]          = $token;
            $last_token             = $token;
            $last_token_type        = $type;
            $last_fortran_data_type = $fortran_data_type;
            if ( $token eq ':' ) { $colon_count++ }

            # Note that type 'vx' (builtin) must not be stored
            if ( $type =~ /^(x|\$|\@|\$x)$/ ) {

                # enter a new symbol in the local symbol table
                if ( !$storage_type ) {

                    # default is that local scalars are type 'save' for now
                    my $save = 1;

                    # parameters are treated as data and always saved
                    if ( $is_parameter{$bareword} ) { $save = 1 }
                    if ( $stmt_type eq 'save' ) { $save      = 1 }
                    if ( $stmt_type eq 'byte' ) { $stmt_type = 'integer' }
                    $local_symbol_table->set(
                        'name'              => $bareword,
                        'storage_type'      => $type,
                        'is_arg'            => $is_arg{$bareword},
                        'save'              => $save,
                        'fortran_data_type' => $fortran_data_type,
                    );
                }

                # update the storage type if changed
                elsif ( $storage_type ne $type ) {
                    $local_symbol_table->set(
                        'name'         => $bareword,
                        'storage_type' => $type,
                    );
                }

                if ( $is_data_type{$stmt_type} && $depth == 0 ) {
                    $fortran_data_type = $stmt_type;
                    $local_symbol_table->set(
                        'name'              => $bareword,
                        'fortran_data_type' => $fortran_data_type,
                    );
                }

                if ($i_equals) {
                    if ( $saw_lv_int && $fortran_data_type !~ /^integer/ ) {
                        $saw_rv_float = 1;
                    }
                }
                else {
                    if ( $depth == 0 && $fortran_data_type =~ /^integer/ ) {
                        $saw_lv_int = 1;
                    }
                }
            }
            else {
                if ( $saw_lv_int && $i_equals ) {
                    unless ( $type =~ /[\=\,\+\-\*\(\)]/ || $token =~ /^\d+$/ )
                    {
                        $saw_rv_float = 1;
                    }
                }
            }
        }

        # Convert rhs to int if necessary.
        if ( $saw_lv_int && $i_equals && $saw_rv_float ) {
            my $i_closing = $max_token_index + 1;

            # In a do stmt, the closing paren will not be at the end of line,
            # but rather before the comma:
            # i.e.     do i=5.5,10
            if ( $stmt_type eq 'do' ) {
                $i_closing = find_next_comma($i_equals);
            }
            $store_inserted_tokens->(
                $i_equals + 1,
                [ 'int', '(' ],
                [ 'vx',  '(' ]
            );
            $store_inserted_tokens->( $i_closing, [')'], [')'] );
        }

        # Now change any tokens that need changing.
        foreach (@changeme) {
            my ( $i, $token ) = @$_;
            $rtokens->[$i] = $rtoken_types->[$i] = $token;
        }

        # Now insert any new tokens.  We have to insert from the highest
        # index to the lowest so that old indexes remain valid.
        my @keys = reverse sort { $a <=> $b } keys %insert_before;
        foreach (@keys) {
            my ( $rinsert_tokens, $rinsert_token_types ) =
              @{ $insert_before{$_} };
            splice @$rtokens,      $_, 0, @$rinsert_tokens;
            splice @$rtoken_types, $_, 0, @$rinsert_token_types;
        }

        # --------------------------------------
        # Done with first pass through this line
        # Go back and do any special processing
        # --------------------------------------

        # We have to do this first so that arrays get decorated
        # Otherwise, introducing 'substr' in process_character_subscripts will
        # cause that to get skipped. Better yet might be to decorate all
        # arrays above.
        if ($saw_subcall) { process_call_args() }

        # These two operations are be mutually exclusive
        # since a ':' may either appear in dimension
        # statements and as a character subscript, but not both.
        if    ($saw_dimensions) { scan_dimensions() }
        elsif ($colon_count)    { process_character_subscripts($colon_count) }

        ## TESTING: iolists must be processed as if they were call parameters
        ## if ($stmt_type =~ /^(write|print)$/) {$saw_subcall=1}

        if ($saw_statement_function) { scan_statement_function() }

        if    ( $stmt_type eq 'parameter' )   { scan_parameter() }
        elsif ( $stmt_type eq 'data' )        { scan_data() }
        elsif ( $stmt_type eq 'equivalence' ) { scan_equivalence() }

        if ( $stmt_type eq 'common' ) { scan_common() }

        # scan_data does own decoration of common scalars
        elsif ( $stmt_type ne 'data' && !$is_declaration{$stmt_type} ) {
            scan_common_scalars();
        }
    }

    sub scan_statement_function {

        # Given an statement function such as this:
        # isum(j,k) = j*5+k-1
        #
        # Create an anonymous sub like this:
        # my $isum = sub { my ($j,$k) = @_; return $j*5+$k-1;}

        # On entry, the token stream will look like this:
        # $isum->(j,k) = $j*5+$k-1

        my $max_token_index = @$rtokens - 1;
        my $i;
        my @new_tokens;
        my @new_token_types;
        my $push_token = sub {
            my ( $tok, $typ ) = @_;
            push @new_tokens,      $tok;
            push @new_token_types, $typ;
        };

        # skip up to the function name
        for ( $i = 0 ; $i <= $max_token_index ; $i += 1 ) {
            last if ( $rtoken_types->[$i] eq '$x' );
        }
        if ( $i > $max_token_index ) {
            error("Syntax error in statement function definition\n");
            return;
        }

        $rtokens->[$i] =~ s/->$//;
        $push_token->( 'my', 'k' );
        $push_token->( ' ',  'b' );
        $push_token->( $rtokens->[$i], $rtoken_types->[$i] );
        $push_token->( ' ',   'b' );
        $push_token->( '=',   '=' );
        $push_token->( ' ',   'b' );
        $push_token->( 'sub', 'k' );
        $push_token->( '{',   '{' );
        if ( $rtoken_types->[ $i + 1 ] ne '(' ) {
            error("Syntax error in statement function definition\n");
            return;
        }
        my $i_end = find_closing_paren( $i + 1 );
        if ( $rtoken_types->[$i_end] ne ')' ) {
            error("Syntax error in statement function definition\n");
            return;
        }
        $push_token->( ' ',  'b' );
        $push_token->( 'my', 'k' );
        $push_token->( ' ',  'b' );
        foreach my $j ( $i + 1 .. $i_end ) {
            $push_token->( $rtokens->[$j], $rtoken_types->[$j] );
        }
        $push_token->( '=',      '=' );
        $push_token->( '@_',     'k' );
        $push_token->( ';',      ';' );
        $push_token->( 'return', 'k' );
        $push_token->( ' ',      'b' );
        if ( $rtoken_types->[ $i_end + 1 ] ne '=' ) {
            error("Syntax error in statement function definition\n");
            return;
        }
        foreach my $j ( $i_end + 2 .. $max_token_index ) {
            $push_token->( $rtokens->[$j], $rtoken_types->[$j] );
        }
        $push_token->( ';', ';' );
        $push_token->( '}', '}' );
        @$rtokens      = @new_tokens;
        @$rtoken_types = @new_token_types;
    }

    sub rename_common_token {

        # add a package name to a token if necessary
        my ($name) = @_;
        my $have_dollar = ( $name =~ s/^\$+// );
        my $word = $name;
        $word =~ s/^(\$?)(C_)?(\w+)(.*)?/$3/;
        my $arrow = $4;
        my $common_package_name =
          $local_symbol_table->get( $word, 'common_package_name' );
        if ($common_package_name) {
            $name = "$common_package_name" . "::" . "$name";
        }
        if ($have_dollar) { $name = '$' . $name }
        return $name;
    }

    sub scan_data {

        # the fortran data statement syntax is:
        #   data nlist /clist/, nlist /clist/ ...
        # where nlist is a list of names and clist is a list of constants
        # and the commas are optional

        my $max_token_index = @$rtokens - 1;
        my $i_start         = 1;
        my $i_beginning_slash;
        my $i_ending_slash;
        my $i_last_ending_slash;
        my $depth = 0;
        for my $i ( 1 .. $max_token_index ) {
            my $type  = $rtoken_types->[$i];
            my $token = $rtokens->[$i];
            if ( $type eq '(' ) { $depth++; next }
            if ( $type eq ')' ) { $depth--; next }
            if ( $depth == 0 && $type eq '/' ) {
                if ( !defined($i_beginning_slash) ) {
                    $i_beginning_slash = $i;
                }
                else {

                    # loop to process the next nlist /clist/ pair
                    $i_ending_slash = $i;
                    scan_data_list_pair( $i_start, $i_beginning_slash,
                        $i_ending_slash );
                    $i_start = $i_ending_slash + 1;
                    if (   $i_start <= $max_token_index
                        && $rtoken_types->[$i_start] eq ',' )
                    {
                        $i_start++;
                    }
                    $i_beginning_slash   = undef;
                    $i_last_ending_slash = $i_ending_slash;
                    $i_ending_slash      = undef;
                }
            }
        }
        if ( !defined($i_last_ending_slash)
            || $i_last_ending_slash != $max_token_index )
        {
            error("Syntax error in data statement\n");
        }
    }

    sub get_range_triplet {
        my ($rterms) = @_;
        my $rrange_triplet;
        my $it1 = @$rterms - 2;

        # look for a range either without or with an increment
        foreach my $it ( $it1, $it1 - 1 ) {
            last if ( $it < 1 );
            my ( $ib, $ie ) = @{ $rterms->[$it] };

            ##my $txt = join_tokens( $ib, $ie );
            ##print DEBUG: checking it=$it ib=$ib ie=$ie text=$txt\n";

            # any equals must be the second token; this rule keeps us
            # from looking into sublists
            my $i_equals = $ib + 1;
            if ( $i_equals < $ie && $rtoken_types->[$i_equals] eq '=' ) {

                # pull out the variables in both symbolic and evaluated forms
                my $do_var          = join_tokens( $ib, $i_equals - 1 );
                my $cannot_evaluate = 0;
                my $sym_begin       = join_tokens( $i_equals + 1, $ie );
                my $val_begin       = get_evaluated_term( $i_equals + 1, $ie );
                if ( !defined($val_begin) ) { $cannot_evaluate = 1 }
                my $sym_end = join_tokens( @{ $rterms->[ $it + 1 ] } );
                my $val_end = get_evaluated_term( @{ $rterms->[ $it + 1 ] } );
                if ( !defined($val_end) ) { $cannot_evaluate = 1 }
                my $sym_inc = '1';
                my $val_inc = 1;

                if ( $it ne $it1 ) {
                    $sym_inc = join_tokens( @{ $rterms->[ $it + 2 ] } );
                    $val_inc = get_evaluated_term( @{ $rterms->[ $it + 2 ] } );
                    if ( !defined($val_inc) ) { $cannot_evaluate = 1; }
                    pop @$rterms;
                }
                pop @$rterms;
                pop @$rterms;
                if ( $cannot_evaluate && $stmt_type eq 'data' ) {
                    error(
"Error in implied-do construct; non-integer or invalid limits for dummy variable $do_var\n"
                    );
                    last;
                }

                my $count = 0;
                my $sym_count;
                if ( $val_inc && $val_inc eq 1 ) {
                    $sym_count = "$sym_end - $sym_begin + 1";
                    unless ($cannot_evaluate) {
                        $count = $val_end - $val_begin + 1;
                    }
                }
                else {
                    if ($cannot_evaluate) {
                        $sym_count = "1 + int(($sym_end-$sym_begin)/$sym_inc)";
                    }
                    else {
                        $count =
                          1 + int( ( $val_end - $val_begin ) / $val_inc );
                        $sym_count = $count;
                    }
                }
                $rrange_triplet = [
                    $do_var,  $sym_begin, $sym_end,
                    $sym_inc, $sym_count, $val_begin,
                    $val_end, $val_inc,   $count
                ];
                ##print "DEBUG term=$dummy_var beg=$sym_begin end=$sym_end inc=$sym_inc count = $count\n";
                last;
            }
        }
        return $rrange_triplet;
    }

    sub scan_name_list {

        # Parse the all the names between token 'ibeg' and 'iend'.
        # The names may be on a data, read, or write statement.
        # Here's an example for a data statement, which has two lists of
        # names to be parsed:
        #
        #   DATA J/1/,(DELTAM(I),I=1,20),(DELTAP(I),I=1,20)/40*0./
        #             ^                                   ^
        #             |                                   |
        #            ibeg                               iend
        #
        # We will store the generated code in an array.  This is a
        # recursive routine because there may be lists within lists.

        my (
            $depth,             $list_obj,     $ibeg,
            $iend,              $rerror,       $rshift_mode,
            $rname_list_output, $rsimple_list, $rsimple_list_ok
          )
          = @_;
        $depth++;
        my $total_item_count = 0;
        my $for_loop_count   = 1;

        # Split at highest level commas into individual terms
        my @terms = split_csv_list( $ibeg, $iend );

        # get the range triplet, if any
        my $rtriplet = get_range_triplet( \@terms );

        ## FUTURE: push variables onto @simple_list unless you
        ## encounter a for loop or an array
##        my $is_simple_list=0;
##        if ( !$rtriplet && $depth == 1 ) {
##            foreach my $term (@terms) {
##                my ( $ib, $ie ) = @$term;
##                last if ( $rtoken_types->[$ib] eq '(' );
##
##                # if this is an array, it could need '@' sign with slice
##
##            }
##            $is_simple_list = 1;
##            my $rhs = join_tokens( $ibeg, $iend );
##            error( "DEBUG simple list ($rhs)\n");
##        }

        # if there is a range triplet, output a 'for' loop
        # TODO: we should check to see if this is really necessary
        if ($rtriplet) {

            $$rsimple_list_ok = 0;
            my (
                $var,       $sym_begin, $sym_end, $sym_inc, $sym_count,
                $val_begin, $val_end,   $val_inc, $count
              )
              = @$rtriplet;
            $for_loop_count = $count;

            # Use 'shift_mode' for any loops.  In shift mode,
            # the data are placed in the '@_' array, and shifted
            # off in a 'for' loop.  For example, the following data
            # statement:
            #
            # DATA (X(1,J),Y(1,J),J=1,3) /0D0,0D0, 6D0,0D0, 3D0,4D0 /
            # END
            #
            # would become:
            # @_ = ( 4e0, 3e0, 0e0, 6e0, 0e0, 0e0, );
            # for ( my $j = 1 ; $j <= 3 ; $j += 1 ) {
            #       $x->( 1, $j ) = shift @_;
            #       $y->( 1, $j ) = shift @_;
            # }
            #
            # If all of the constants are the same, the '@_' list
            # is eliminated and the items are initialized directly
            # to the common value. Thus, in the following example:
            #
            #   DATA J/1/,(DELTAM(I),I=1,20),(DELTAP(I),I=1,20)/40*0./
            #
            # becomes simply:
            #
            #   $j = 1;
            #   for ( my $i = 1 ; $i <= 20 ; $i += 1 ) {
            #       $deltam[ $i - 1 ] = 0.;
            #   }
            #   for ( my $i = 1 ; $i <= 20 ; $i += 1 ) {
            #       $deltap[ $i - 1 ] = 0.;
            #   }

            if ( !defined($$rshift_mode) ) {
                if ( !$list_obj->count() ) {
                    error(
                        "More variables than initial values in data statement\n"
                    );
                    $$rerror = 1;
                    return;
                }
                my $shift_count;

                # We will enter permanent shift mode if this list
                # contains any sublists.  In this case, which is not too
                # common, not as much checking will be done.  We'll just
                # assume that the original fortran coding is ok.
                foreach my $term (@terms) {
                    my ( $ib, $ie ) = @$term;
                    if ( $rtoken_types->[$ib] eq '(' ) {
                        $shift_count = $list_obj->count();
                        last;
                    }
                }

                # otherwise, we will just take as many constants
                # as we need
                if ( !$shift_count ) {
                    $shift_count = $count * @terms;
                    if ( $shift_count > $list_obj->count() ) {
                        $shift_count = $list_obj->count();
                        error(
"More variables than initial values in data statement\n"
                        );
                    }
                }

                my $val = $list_obj->get_list_or_constant($shift_count);

                # Handle the case of different list values
                if ( ref $val ) {
                    my $rhs = '(' . join( ', ', @$val ) . ')';
                    push( @$rname_list_output, "\@_=$rhs;" );
                    $$rshift_mode = 'shift @_';
                }

                # Handle case of all list items the same
                else {
                    $$rshift_mode = $val;
                }
            }

            # Now we can write the 'for' loop
            push( @$rname_list_output,
                "for (my $var=$sym_begin ; $var <= $sym_end; $var+=$sym_inc) {"
            );
        }

        # Now we can scan and process all of the terms in this list
        foreach my $term (@terms) {
            my ( $ib, $ie ) = @$term;

            # Check for a sublist.  Careful..  Here's an example of
            # something within parens that is not a sublist:
            # WRITE(TSTRNG,'(1PE12.5)') (TSTRT+TLENG)*10**(3*KTLABL)
            if ( $rtoken_types->[$ib] eq '(' && $ie == find_closing_paren($ib) )
            {
                my $tic = scan_name_list(
                    $depth,             $list_obj,
                    $ib + 1,            $ie - 1,
                    $rerror,            $rshift_mode,
                    $rname_list_output, $rsimple_list,
                    $rsimple_list_ok
                );
                $total_item_count += $tic * $for_loop_count;
                return $total_item_count if $$rerror;
            }

            # scan a regular term
            else {
                my $name = join "", @$rtokens[ $ib .. $ie ];
                my $tic =
                  write_name_data_pair( $name, $list_obj, $depth, $rerror,
                    $rshift_mode, $rname_list_output, $rsimple_list,
                    $rsimple_list_ok );
                $total_item_count += $tic * $for_loop_count;
                return $total_item_count if $$rerror;
            }
        }

        # close any 'for' loop
        if ($rtriplet) {
            push( @$rname_list_output, '}' );
        }

        # Exit shift mode if there are remaining constants.
        # (In permanent shift mode, there are no remining constants)
        if ( defined($$rshift_mode) && $list_obj->count() ) {
            $$rshift_mode = undef;
        }
        return $total_item_count;
    }

    sub write_name_data_pair {
        my ( $name, $list_obj, $depth, $rerror, $rshift_mode,
            $rname_list_output, $rsimple_list, $rsimple_list_ok )
          = @_;
        my $is_valid   = 0;
        my $bareword   = $name;
        my $item_count = 1;

        my $error = sub {
            my ($msg) = @_;
            error($msg);
            $$rerror = 1;
        };

        my $initialize_scalar = sub {
            my ($name) = @_;
            if ( defined($$rshift_mode) ) {
                if ( $stmt_type eq 'write' || $stmt_type eq 'print' ) {
                    push( @$rname_list_output, $$rshift_mode . $name . ';' );
                    if ($$rsimple_list_ok) { push @$rsimple_list, $name }
                }
                else {
                    push( @$rname_list_output, "$name = $$rshift_mode;" );
                    if ($$rsimple_list_ok) { push @$rsimple_list, $name }
                }
            }
            elsif ( $list_obj->count() ) {
                my $iv = $list_obj->get_next();
                push( @$rname_list_output, "$name=$iv;" );
                if ($$rsimple_list_ok) { push @$rsimple_list, $name }
            }
            else {
                $error->(
                    "More variables than initial values in data statement\n");
            }
        };

        # Handle a scalar and a single array item
        if ( $name =~ /^\$/ ) {

            $bareword =~ s/^\$+//;
            if ( $bareword =~ /^\w+::(\w+)/ ) { $bareword = $1 }

            if ( $bareword =~ /->/ ) {

                ## TESTING: allow sub calls (common variables)
                $is_valid = 1;
            }
            elsif ( $bareword =~ /^(\w+)([\[\(].*)?/ ) {

                # FIXME: more checks needed here; for example, it
                # connot be an arg,
                # and arrays need work
                # This gets very tricky because scan_barewords has already
                # been over this line; the order may be wrong

                $is_valid = 1;
                if ( $stmt_type eq 'data' ) {
                    $bareword = $1;
                    my $arg          = $2;
                    my $storage_type =
                      $local_symbol_table->get( $bareword, 'storage_type' );
                    if ($storage_type) {
                        $local_symbol_table->set(
                            'name' => $bareword,
                            'save' => 1,
                        );
                    }
                    else {
                        $is_valid = 0;
                    }
                }
            }
            unless ($is_valid) {
                $error->("error parsing data statement at ($name)\n");
                return $item_count;
            }
            $name = decorate_common_scalar($name);
            $initialize_scalar->($name);
        }
        elsif ( $name =~ /^substr\(/ ) {

            ## TODO: should mark variable after opening paren as saved
            ## although not really necessary yet
            $initialize_scalar->($name);
        }
        else {
            my $storage_type =
              $local_symbol_table->get( $name, 'storage_type' );

            if ( !defined($storage_type) || $storage_type ne '@' ) {

                # assume it is a constant or expression in write mode
                if ( $stmt_type eq 'write' || $stmt_type eq 'print' ) {
                    $initialize_scalar->($name);
                }
                else {

                    if ( !$storage_type ) {
                        $error->(
                            "$name is undefined but is used as an array here\n"
                        );
                    }
                    elsif ( $storage_type eq '$' ) {
                        $error->(
                            "$name is a scalar but is used as an array here\n");
                    }
                    else {
                        $error->(
"$name has not been declared as an array but is being used as one here\n"
                        );
                    }
                }
                return $item_count;
            }

            if ( $stmt_type eq 'data' ) {
                $local_symbol_table->set(
                    'name' => $name,
                    'save' => 1,
                );
            }
            my $array_size = $local_symbol_table->get( $name, 'array_size' );

            if ( !defined($array_size) ) {
                $error->("$name used as array but has not been declared\n");
                $array_size = defined($$rshift_mode) ? "UNKNOWN" : 1;
            }

            if ( defined($$rshift_mode) ) {
                $$rsimple_list_ok = 0;
                my $rimaxs = $local_symbol_table->get( $name, 'imaxs' );
                my $rimins = $local_symbol_table->get( $name, 'imins' );
                my $subscript;
                if ( use_simple_access($name) ) {
                    $subscript = '[$I1-1]';
                }
                else {
                    $subscript =
                      '->(' . '$I' . join( ', $I', 1 .. @$rimaxs ) . ')';
                }
                for ( my $i = @$rimaxs ; $i >= 1 ; $i-- ) {
                    my $min = $rimins->[ $i - 1 ];
                    my $max = $rimaxs->[ $i - 1 ];
                    push( @$rname_list_output,
                        "for (my \$I$i = $min; \$I$i <= $max; \$I$i++) {" );
                }

                if ( $stmt_type eq 'write' || $stmt_type eq 'print' ) {
                    push( @$rname_list_output,
                        $$rshift_mode . "\$$name" . $subscript . ';' );
                }
                else {
                    push( @$rname_list_output,
                        "\$$name" . "$subscript = $$rshift_mode;" );
                }
                foreach ( 1 .. @$rimaxs ) {
                    push( @$rname_list_output, '}' );
                }
            }
            elsif ( $list_obj->count() ) {
                $item_count = $list_obj->count();
                if ( $item_count > $array_size ) {
                    $item_count = $array_size;
                }
                my $val = $list_obj->get_list_or_constant($item_count);
                my $rhs;
                if ( ref $val ) {
                    $rhs = '(' . join( ', ', @$val ) . ')';
                }
                else {
                    $rhs = "($val) x $item_count";
                }
                my $item_count_m = $item_count - 1;
                push( @$rname_list_output,
                    "\@$name" . "[ 0 .. $item_count_m ] = $rhs;" );
                if ($$rsimple_list_ok) {
                    push( @$rname_list_output,
                        "\@$name" . "[ 0 .. $item_count_m ]" );
                }
            }
            else {
                $error->("Not enough values to initialize $bareword\n");
            }
        }
        return $item_count;
    }

    sub scan_data_list_pair {

        my ( $i_start, $i_beginning_slash, $i_ending_slash ) = @_;
        my $error = 0;
        my $shift_mode;
        my $rconstants =
          scan_data_constants( $i_beginning_slash + 1, $i_ending_slash - 1 );
        my $list_obj         = new Fortran::F77toPerl::Lister($rconstants);
        my $depth            = 0;
        my @name_list_output = ();
        my $constant_count   = $list_obj->count();
        my @simple_list      = ();
        my $simple_list_ok   = 0;
        my $total_item_count = scan_name_list(
            $depth,             $list_obj,
            $i_start,           $i_beginning_slash - 1,
            \$error,            \$shift_mode,
            \@name_list_output, \@simple_list,
            \$simple_list_ok
        );

        if ( !$error && $total_item_count != $constant_count ) {
            error(
"number of initial values is $constant_count but need $total_item_count\n"
            );
        }
        foreach (@name_list_output) { write_data($_) }
    }

    sub scan_data_constants {
        my ( $ibeg, $iend ) = @_;
        my @constants;
        my @terms = split_csv_list( $ibeg, $iend );
        foreach my $term (@terms) {
            my ( $ib, $ie ) = @$term;
            my $i_star;
            for ( my $i = $ib + 1 ; $i < $ie ; $i++ ) {
                if ( $rtoken_types->[$i] eq '*' ) {
                    $i_star = $i;
                    last;
                }
            }
            if ($i_star) {
                my $const = join_tokens( $i_star + 1, $ie );
                my $count = get_evaluated_term( $ib, $i_star - 1 );
                if ($count) {
                    foreach ( 1 .. $count ) {
                        push @constants, $const;
                    }
                }
                else {
                    error("error evaluating multiplier of $const");
                }
            }
            else {
                push @constants, join_tokens( $ib, $ie );
            }
        }
        return wantarray ? @constants : \@constants;
    }

    sub scan_parameter {

        my $i_start         = 1;
        my $max_token_index = @$rtokens - 1;
        if ( $max_token_index < 2 || $rtoken_types->[$i_start] ne '(' ) {
            error("syntax error at paramter statement\n");
            return;
        }

        my $i_end = find_closing_paren($i_start);
        if (   $i_end != $max_token_index
            || $rtoken_types->[$i_end] ne ')' )
        {
            error("syntax error at paramter statement\n");
            return;
        }

        my $i_next_comma = 1;
        my $i_comma;
        while (1) {
            $i_comma      = $i_next_comma;
            $i_next_comma = find_next_comma( $i_comma + 1 );
            eval_parameter( $i_comma, $i_next_comma );
            last if ( $rtoken_types->[$i_next_comma] ne ',' );
        }
    }

    # ----------------------------------------------------------------
    # sub max and min are not called directly but may be required by the
    # parameter 'eval's.  Include any other non-perl routines here as
    # necessary.
    # ----------------------------------------------------------------
    sub max {
        my $max = shift;
        foreach (@_) {
            $max = ( $max < $_ ) ? $_ : $max;
        }
        return $max;
    }

    sub min {
        my $min = shift;
        foreach (@_) {
            $min = ( $min > $_ ) ? $_ : $min;
        }
        return $min;
    }

    sub eval_parameter {

        # Evaluate a parameter, possibly continaing other parameters,
        # to get a value.  Update the parameter tables.
        my ( $i_comma, $i_next_comma ) = @_;
        my $token    = $rtokens->[ $i_comma + 1 ];
        my $bareword = $token;
        $bareword =~ s/^\$+//;
        unless ( $is_parameter{$bareword} ) {
            error("syntax error at paramter statement near $bareword\n");
            return;
        }

        # store the parameter definition
        if ( defined( $parameter_symbolic_value{$bareword} ) ) {
            print STDERR "Attempting to redefine parameter $bareword\n";
            return;
        }
        else {
            push @parameter_name_order, $bareword;
            $parameter_dependents{$bareword} = [];

            # NOTE: it would be best to do this at the end of the
            # subroutine, and only for parameters actually used
            my $rhs = join_tokens( $i_comma + 3, $i_next_comma - 1 );
            write_parameter_data("$token = $rhs;");
        }

        # now find its actual value
        my $eval_string    = "";
        my $symbolic_value = "";
        foreach my $i ( $i_comma + 3 .. $i_next_comma - 1 ) {
            my $tok = $rtokens->[$i];
            if ( $rtoken_types->[$i] eq '$' ) {
                my $word = $tok;
                $word =~ s/^\$+//;
                $symbolic_value .= $word;
                if ( defined( $parameter_nonsymbolic_value{$word} ) ) {
                    $eval_string .= $parameter_nonsymbolic_value{$word};
                    push @{ $parameter_dependents{$bareword} }, $word;
                }
                else {
                    print STDERR
"unexpected token \$$word in definition of parameter $bareword\n";
                }

            }
            else {
                $eval_string    .= $tok;
                $symbolic_value .= $tok;
            }
        }

        $parameter_symbolic_value{$bareword}    = $symbolic_value;
        $parameter_nonsymbolic_value{$bareword} = $symbolic_value;

        my $value = eval($eval_string);
        if ($@) {

            # This isn't necessarily an error -- for example, it might
            # be a fortran character concatenation expression.  We will
            # just leave the non symbolic value undefined.
            # However, this might cause problems in sub evaluate_term;
            # Perhaps more functions like 'min' and 'max' are needed.
            # Maybe remove this after debugging.
            print STDERR <<EOM;
DEBUG: Unable to evaluate this expression for $bareword:
$eval_string
EOM
        }
        else {
            $parameter_nonsymbolic_value{$bareword} = $value;
            ##print "DEBUG parameter $bareword string=$eval_string = $value; \n";
        }
    }

    sub split_colon_dimensions {

        # get an array dimension, and split it on a ':' if necessary
        my ( $ibeg, $iend ) = @_;

        my @dependencies;
        my $imin_value;
        my $imax_value;

        my ( $a, $b, $c, $d ) = split_and_eval( $ibeg, $iend, ':' );
        my $count = @$a;
        if ( $count == 1 ) {
            $imin_value = 1;
            $imax_value = $a->[0];
            if ( $d->[0] ) {
                @dependencies = @{ $d->[0] };
            }
        }
        elsif ( $count == 2 ) {
            $imin_value = $a->[0];
            $imax_value = $a->[1];
            if ( $d->[0] ) {
                @dependencies = @{ $d->[0] };
            }
            if ( $d->[1] ) {
                push @dependencies, @{ $d->[1] };
            }
        }
        else {
            error("Error evaluating array dimensions\n");
        }

        # try to make a clean symbolic array size
        my $dim_dollar = join_tokens( $ibeg, $iend );
        my $imax       = $dim_dollar;
        my $imin       = 1;
        if ( $imax =~ /:/ ) { ( $imin, $imax ) = split ':', $imax; }
        if ( $imin eq '1' ) {
            $dim_dollar = $imax;
        }
        elsif ( $imin =~ /^[\+\-\d]+$/ ) {
            my $tmp = $imin - 1;
            if ( $tmp > 0 ) {
                $dim_dollar = "$imax - $tmp";
            }
            else {
                $tmp        = -$tmp;
                $dim_dollar = "$imax + $tmp";
            }
        }
        else {
            $dim_dollar = "$imax - $imin + 1";
        }

        if ( $dim_dollar =~ /^[\+\-\d\s]+$/ ) {
            $dim_dollar = eval($dim_dollar);
        }
        elsif ( $dim_dollar =~ /[\+\-\s]/ ) {
            $dim_dollar = "($dim_dollar)";
        }
        return ( $imin, $imax, $imin_value, $imax_value, $dim_dollar,
            \@dependencies );
    }

    sub split_and_eval {

        # split current token list at the highest level.
        # split on ',' unless split_token is given
        # return evaluated and symbolic items and parameter
        # dependencies.
        my ( $i_start, $i_end, $split_token ) = @_;
        unless ( defined($split_token) ) { $split_token = ',' }
        my @evaluated_terms;
        my @symbolic_terms;
        my @symbolic_terms_no_dollar;
        my @dependencies;
        my $starting_depth = 0;
        my $depth          = 0;
        my $i;
        my $max_token_index = @$rtoken_types - 1;
        if ( !defined($i_end) )          { $i_end = $max_token_index }
        if ( $i_end > $max_token_index ) { $i_end = $max_token_index }
        my $i_begin = $i_start;
        if ( $rtoken_types->[$i_begin] =~ /^[\,\(]$/ ) { $i_begin++ }

        for ( $i = $i_start ; $i <= $i_end ; $i++ ) {
            my $type = $rtoken_types->[$i];
            if ( $type eq ')' ) {
                $depth--;
                if ( $depth < $starting_depth ) {
                    last;
                }
            }
            elsif ( $type eq '(' ) {
                $depth++;
            }
            elsif ( $type eq $split_token ) {
                if ( $depth == $starting_depth ) {
                    my (
                        $evaluated_term,          $symbolic_term,
                        $symbolic_term_no_dollar, $rdependencies
                      )
                      = evaluate_term( $i_begin, $i - 1 );
                    push @symbolic_terms,           $symbolic_term;
                    push @symbolic_terms_no_dollar, $symbolic_term_no_dollar;
                    push @evaluated_terms,          $evaluated_term;
                    push @dependencies,             $rdependencies;
                    $i_begin = $i + 1;
                }
            }
            if ( $i == $i_start ) { $starting_depth = $depth }
        }
        if ( $i > $i_begin ) {
            my (
                $evaluated_term,          $symbolic_term,
                $symbolic_term_no_dollar, $rdependencies
              )
              = evaluate_term( $i_begin, $i - 1 );
            push @symbolic_terms,           $symbolic_term;
            push @symbolic_terms_no_dollar, $symbolic_term_no_dollar;
            push @evaluated_terms,          $evaluated_term;
            push @dependencies,             $rdependencies;
        }
        return (
            \@evaluated_terms,          \@symbolic_terms,
            \@symbolic_terms_no_dollar, \@dependencies
        );
    }

    sub get_evaluated_term {

        # look at tokens from ibeg to iend and return
        # the term evaluated to a numerical value
        my ( $ibeg, $iend ) = @_;
        my ( $value, $symbolic_term, $symbolic_term_no_dollar, $rdependencies )
          = evaluate_term( $ibeg, $iend );
        return $value;
    }

    sub evaluate_term {

        # look at tokens from ibeg to iend and return in 3 forms:
        # - the term evaluated to a numerical value
        # - the symbolic term (that is, just the join of them)
        # - the symbolic term without any dollars in parameters
        # also return list of parameter dependencies
        my ( $ibeg, $iend ) = @_;
        my $eval_str                = "";
        my $symbolic_term           = "";
        my $symbolic_term_no_dollar = "";
        my $cannot_evaluate         = 0;
        my @dependencies;
        my $tok;
        my $type;

        foreach my $i ( $ibeg .. $iend ) {
            $tok  = $rtokens->[$i];
            $type = $rtoken_types->[$i];

            # We can probably evaluate a parameter
            if ( $type eq '$' ) {
                $symbolic_term .= $tok;
                my $bareword = $tok;
                $bareword =~ s/^\$+//;
                if ( $is_parameter{$bareword} ) {
                    push @dependencies, $bareword;
                    $symbolic_term_no_dollar .= $bareword;
                    $eval_str .= $parameter_nonsymbolic_value{$bareword};
                }
                else {
                    $cannot_evaluate = 1;
                    $symbolic_term_no_dollar .= $tok;
                    $eval_str                .= $tok;
                }
            }

            #  But we can't do anything with an array element or
            #  function call.  Example of loop limits which are arrays
            #  and cannot be evaluated:
            #  read (irezno) (sumext(nexv), nexv=nexvst(0),nexven(0))
            elsif ( $type eq '@' || $type =~ /x$/ ) {
                $cannot_evaluate = 1;
                $symbolic_term_no_dollar .= $tok;
                $eval_str                .= $tok;
            }
            else {
                $symbolic_term           .= $tok;
                $symbolic_term_no_dollar .= $tok;
                $eval_str                .= $tok;
            }
        }
        ##print "DEBUG eval_str=$eval_str cannot=$cannot_evaluate\n";

        my $evaluated_term;
        if ($cannot_evaluate) {
            $evaluated_term = undef;
        }
        elsif ( $eval_str eq '*' ) {
            $evaluated_term = undef;
        }
        elsif ( $eval_str =~ /^\d+$/ ) {
            $evaluated_term = $eval_str;
        }
        else {
            $evaluated_term = eval($eval_str);

            # This can happen if we cannot evaluate the parameter
            # in sub eval_parameter;
            if ($@) {
                ## DEBUG:
                ##my ($a,$b,$c)=caller();
                ##my ($d,$e,$f)=caller(1);
                print STDERR "Error evaluating imax for string '$eval_str' \n";
                ##"Error evaluating imax for string '$eval_str' $a $c $d $f\n";
            }
        }
        return (
            $evaluated_term,          $symbolic_term,
            $symbolic_term_no_dollar, \@dependencies
        );
    }

    sub scan_common {

        # scan a common statement just to get the order of names
        # and do basic checking
        my $max_token_index = @$rtokens - 1;
        my $depth           = 0;
        my $common_name     = "_";
        my $i_start         = 1;
        if ( $max_token_index >= 3 && $rtoken_types->[2] eq 'n' ) {
            $common_name = $rtokens->[2];
            $i_start     = 4;
        }

        for ( my $i = $i_start ; $i <= $max_token_index; $i+=1 ) {
            my $type  = $rtoken_types->[$i];
            my $token = $rtokens->[$i];
            if ( $type eq '(' ) { $depth++; next }
            if ( $type eq ')' ) { $depth--; next }
            ##print "DEBUG: i=$i type=$type tok=$token\n";

            # Check for new common name within statement
            # common2.f
            if ( $type eq '/' ) {
                if (   $i + 2 <= $max_token_index
                    && $rtoken_types->[ $i + 2 ] eq '/' )
                {
                    # Patch - it's already been given a dollar sign
                    $rtokens->[$i+1] =~ s/^\$//;
                    $rtoken_types->[$i+1] = 'n';
                    $common_name = $rtokens->[ $i + 1 ];
                    $i = $i + 2;
                }
                else {
                    error("unexpected / in common $common_name\n");
                }
                next;
            }
            next unless $depth == 0;
            if ( $type eq '$' || $type eq '@' ) {
                my $bareword = $token;
                $bareword =~ s/^\$+//;
                if ( $is_arg{$bareword} ) {
                    error("$bareword is both a call arg and in common\n");
                }
                push @{ $rcommon_variables{$common_name} }, $bareword;
                my $is_common = my $badboy =
                  $local_symbol_table->get( $bareword, $common_name );
                if ($badboy) {
                    error("$bareword is already in common $badboy\n");
                }
                $local_symbol_table->set(
                    'name'        => $bareword,
                    'is_common'   => 1,
                    'common_name' => $common_name,
                );
            }
        }
    }

    sub decorate_common_scalar {

        # given a scalar token, make it a sub call if it is in common
        my ($token) = @_;
        my $word = $token;
        $word =~ s/^\$+//;
        if ( $local_symbol_table->get( $word, 'is_common' ) ) {
            $local_symbol_table->set(
                'name'            => $word,
                'need_access_sub' => 1
            );
            $token = "\$$word";
            my $common_package_name =
               $local_symbol_table->get( $word, 'common_package_name' );
            if ($common_package_name) {
               $token = rename_common_token($token);
            }

            # Must force a call for a scalar
            else {

               # But..if we haven't frozen storage, we don't know the
               # package name and we may get the wrong result.
               if ( !$froze_storage) {
                    if ( $stmt_type eq 'data' ) {
                        error(
"'$word' should be in BLOCKDATA - output code may be wrong \n"
                        );
                        my $common_name =
                          $local_symbol_table->get( $word, 'common_name' );

                        # try to fix it - assume common name is package name
                        if ($common_name) {
                            my $BUBBA = $token;
                            if ( $token =~ s/^\$// ) {
                                $token = '$' . $common_name . '::' . $token;
                            }
                        }
                    }

                    # An error here may mean that scan_common_scalars
                    # should not have been called for this statement type.
                    else {
                        error(
"$word initialized before commons frozen - POSSIBLE PROGRAM BUG\n"
                        );
                    }
               }
               else {
                 $token .= '->()';
               }
            }
        }
        return $token;
    }

    sub scan_common_scalars {

        # we have to decorate common scalars with an '&' to force
        # a sub call
        my $max_token_index = @$rtokens - 1;
        my $depth           = 0;
        my $i_start         = 0;
        if ($stmt_type) { $i_start++ }
        my $last_token;
        my $last_token_type;
        my $token = "";
        my $type  = "";

        for my $i ( $i_start .. $max_token_index ) {
            $type  = $rtoken_types->[$i];
            $token = $rtokens->[$i];
            if ( $type eq '(' ) { $depth++; next }
            if ( $type eq ')' ) { $depth--; next }
            if ( $type eq '$' ) {
                $rtokens->[$i] = decorate_common_scalar($token);
            }
        }
    }

    sub scan_equivalence {

        # parse one equivalance line pair-by-pair
        #
        # equivalence(a(2,2),b(1,3,5)),(c(1),d(6,7)), ...
        #            |^^^^^^^^^^^^^^^|
        #            |               |
        #      i_start               i_end
        #
        my $i_start         = 1;
        my $i_end           = $i_start - 2;
        my $max_token_index = @$rtokens - 1;

        while (1) {

            $i_start = $i_end + 2;
            last if ( $i_start > $max_token_index );

            if ( $max_token_index < 2 || $rtoken_types->[$i_start] ne '(' ) {
                print STDERR
"max=$max_token_index istart=$i_start tok= $rtoken_types->[$i_start]\n";
                error("syntax error at equivalence statement\n");
                return;
            }

            $i_end = find_closing_paren($i_start);
            if ( $rtoken_types->[$i_end] ne ')' ) {
                ##print STDERR "BUB: max=$max_token_index iend=$i_end tok= $rtoken_types->[$i_end]\n";
                error("syntax error at equivalence statement\n");
                return;
            }

            scan_equivalence_pair( $i_start, $i_end );
        }
    }

    sub scan_equivalence_pair {

        # parse one parenthesized equivalance pair such as marked here:
        #
        # equivalence(a(2,2),b(1,3,5)),(c(1),d(6,7))
        #            |^^^^^^|^^^^^^^^|
        #            |      |        |
        #      i_start    i_comma   i_end

        my ( $i_start, $i_end ) = @_;
        my @i_commas = find_commas( $i_start + 1, $i_end - 1 );
        if ( @i_commas < 1 ) {
            print STDERR "Syntax Error in EQUIVALENCE\n";
            return;
        }
        my $i_comma_0 = $i_commas[0];
        my ( $lname, $levaluated_indexes, $lsymbolic_indexes ) =
          get_equivalenced_variable( $i_start + 1, $i_comma_0 - 1 );
        return unless $lname;

        my $i_next_comma = $i_comma_0;
        for ( my $k = 1 ; $k <= @i_commas ; $k++ ) {
            my $i_comma = $i_next_comma;
            if ( $k < @i_commas ) { $i_next_comma = $i_commas[$k] }
            else { $i_next_comma = $i_end }

            my ( $rname, $revaluated_indexes, $rsymbolic_indexes ) =
              get_equivalenced_variable( $i_comma + 1, $i_next_comma - 1 );
            return unless $rname;
            ##print "DEBUG lname is $lname, \n";
            ##print "DEBUG rname is $rname, \n";

            # just save the equivalence information.  it will be resolved
            # after all declarations are seen
            push @equivalences,
              [
                $lname, $levaluated_indexes, $lsymbolic_indexes,
                $rname, $revaluated_indexes, $rsymbolic_indexes
              ];
        }
    }

    sub get_equivalenced_variable {

        # parse one equivalance variable such as marked here:
        #
        # equivalence(a(2,2),b(1,3,5)),(c(1),d(6,7))
        #                    |^^^^^^|
        #                    |      |
        #                i_beg      i_end
        #
        # and check for all possible errors
        #
        my ( $i_beg, $i_end ) = @_;
        my $name = $rtokens->[$i_beg];

        # scalars will already have a '$'
        $name =~ s/^\$+//;
        $name =~ s/->$//;

        my $return_name;
        my ( $rsymbolic_indexes, $revaluated_indexes );
        if ( $i_end > $i_beg ) {

            # EQUIVALENCES: get both symbolic and actual indexes here
            # see split_colon
            ( $revaluated_indexes, $rsymbolic_indexes ) =
              split_and_eval( $i_beg + 2, $i_end - 1 );
            ##TESTING ONLY-return both types
            ##@indexes=@$revaluated_indexes;
        }

        my $is_arg = $local_symbol_table->get( $name, 'is_arg' );
        if ($is_arg) {
            error("Attempt to equivalance call arg $name\n");
            goto RET;
        }
        $return_name = $name;

      RET:
        return ( $return_name, $revaluated_indexes, $rsymbolic_indexes );
    }

    sub layout_common_blocks {

        # Calculate the offsets of all common variables
        # These are needed to help resolve equivalences

        foreach my $common_name ( sort keys %rcommon_variables ) {
            my $offset         = 0;
            my $is_master_copy =
              !defined( $common_symbol_tables{$common_name} );
            foreach my $name ( @{ $rcommon_variables{$common_name} } ) {

                my $package_name = $common_name;
                unless ($is_master_copy) {
                    my $master_offset =
                      $common_symbol_tables{$common_name}
                      ->get( $name, 'common_offset' );
                    unless ( defined($master_offset)
                        && $master_offset == $offset )
                    {
                        $package_name = "";
                        push @{ $rlocal_common_equivalences{$common_name} },
                          $name;
                    }
                }

                $local_symbol_table->set(
                    'name'                => $name,
                    'common_offset'       => $offset,
                    'common_package_name' => $package_name,
                );
                my $storage_type =
                  $local_symbol_table->get( $name, 'storage_type' );
                if ( $storage_type eq '$' ) {
                    $offset += 1;
                }
                elsif ( $storage_type eq '@' ) {
                    my $array_size =
                      $local_symbol_table->get( $name, 'array_size' );
                    $offset += $array_size;
                }
                else {

                    # shouldn't happen
                    error(
"Storage type for $name in common $common_name is $storage_type\n"
                    );
                }
            }
            my $size=$common_block_maximum_size{$common_name};
            unless ($size && $size >= $offset) {
               $common_block_maximum_size{$common_name}=$offset;
            }
        }
    }

    sub resolve_equivalences {
        return unless (@equivalences);

        # check for errors and weed out the bad ones
        my @good;
        foreach my $eqv (@equivalences) {

            my ( $lname, $lindex, $lsymbolic, $rname, $rindex, $rsymbolic ) =
              @$eqv;
            ##print "DEBUG lname is $lname, rname is $rname\n";
            my $lindex_value = eval_equivalence_index( $lname, $lindex );
            my $rindex_value = eval_equivalence_index( $rname, $rindex );
            if ( defined($lindex_value) && defined($rindex_value) ) {

                #
                # TODO: error if fortran data types are different!
                # ie, error if lname is real*4 and rname is real*8
                #
                push @good,
                  [
                    $lname, $lindex, $lsymbolic, $lindex_value,
                    $rname, $rindex, $rsymbolic, $rindex_value
                  ];
            }
        }
        @equivalences = @good;

        find_common_equivalences();

        # the rest are local
        if (@equivalences) {
            make_local_equivalences();
        }
    }

    sub find_common_equivalences {

        # Find all equivalences among common variables store their parameters
        # The actual layout in common gets done later.
        my $more_to_do = 1;
        my $pass       = @equivalences;
        while ($more_to_do) {
            $more_to_do = 0;

            # shouldn't happen because we remove one equivalence per
            # pass
            if ( $pass-- < 0 ) {
                print STDERR "Program bug resolving equivalences\n";
                last;
            }
            foreach my $i ( 0 .. @equivalences - 1 ) {
                my ( $lname, $lindex, $lsymbolic, $lindex_value, $rname,
                    $rindex, $rsymbolic, $rindex_value, )
                  = @{ $equivalences[$i] };
                my $l_is_common =
                  $local_symbol_table->get( $lname, 'is_common' );
                my $r_is_common =
                  $local_symbol_table->get( $rname, 'is_common' );
                if ($l_is_common) {
                    make_common_equivalence( $rname, $rindex, $rindex_value,
                        $lname, $lindex, $lindex_value );
                }
                elsif ($r_is_common) {
                    make_common_equivalence( $lname, $lindex, $lindex_value,
                        $rname, $rindex, $rindex_value );
                }
                else {
                    next;
                }
                splice @equivalences, $i, 1;
                $more_to_do = 1 if @equivalences;
                last;
            }
        }
    }

    sub make_local_equivalences {

        return unless @equivalences;

        # -----------------------------------------------------
        # first find all groups of locally equivalent variables
        # -----------------------------------------------------
        my $current_group = 0;
        my %group_member;
        my @group_members;
        my %relative_offset;

        my ( $lname, $lindex, $lsymbolic, $lindex_value, $rname, $rindex,
            $rsymbolic, $rindex_value );

        my $start_lname_in_new_group = sub {
            $current_group++;
            $group_member{$lname}    = $current_group;
            $relative_offset{$lname} = 0;
            push @{ $group_members[$current_group] }, $lname;
        };

        my $add_rname_to_current_group = sub {
            $group_member{$rname}    = $current_group;
            $relative_offset{$rname} =
              $relative_offset{$lname} + $lindex_value - $rindex_value;
            push @{ $group_members[$current_group] }, $rname;
        };

        my $add_lname_to_current_group = sub {
            $group_member{$lname}    = $current_group;
            $relative_offset{$lname} =
              $relative_offset{$rname} + $rindex_value - $lindex_value;
            push @{ $group_members[$current_group] }, $lname;
        };

        my $more_to_do = 1;
        while (@equivalences) {

            # start a new group with the first pair in the list
            my $eqv = shift(@equivalences);
            (
                $lname, $lindex, $lsymbolic, $lindex_value, $rname, $rindex,
                $rsymbolic, $rindex_value,
              )
              = @$eqv;
            $start_lname_in_new_group->();
            $add_rname_to_current_group->();

            # loop to pick up all other members of this group
            my $more_to_do = 1;
            while ($more_to_do) {
                $more_to_do = 0;
                foreach my $i ( 0 .. @equivalences - 1 ) {

                    (
                        $lname, $lindex, $lsymbolic, $lindex_value, $rname,
                        $rindex, $rsymbolic, $rindex_value,
                      )
                      = @{ $equivalences[$i] };
                    if (   $group_member{$lname}
                        && $group_member{$lname} == $current_group )
                    {
                        $add_rname_to_current_group->();
                    }
                    elsif ($group_member{$rname}
                        && $group_member{$rname} == $current_group )
                    {
                        $add_lname_to_current_group->();
                    }
                    else {
                        next;
                    }
                    splice @equivalences, $i, 1;
                    $more_to_do = 1 if @equivalences;
                    last;
                }
            }
        }

        # ------------------------------------------------------
        # now, for each group of locally equivalenced variables,
        # pick the one with the minimum offset as the base name
        # ------------------------------------------------------
        for ( my $i = 1 ; $i <= $current_group ; $i++ ) {
            my @members           = @{ $group_members[$i] };
            my $base_member       = $members[0];
            my $min_offset        = $relative_offset{$base_member};
            my $base_storage_type =
              $local_symbol_table->get( $base_member, 'storage_type' );
            foreach ( @members[ 1 .. @members - 1 ] ) {
                my $offset = $relative_offset{$_};
                if ( $offset < $min_offset ) {
                    $base_member       = $_;
                    $min_offset        = $offset;
                    $base_storage_type =
                      $local_symbol_table->get( $base_member, 'storage_type' );
                }

                # in case of equal offsets, we have to take the array
                elsif ( $offset == $min_offset && $base_storage_type ne '@' ) {
                    my $storage_type =
                      $local_symbol_table->get( $_, 'storage_type' );
                    if ( $storage_type eq '@' ) {
                        $base_member       = $_;
                        $min_offset        = $offset;
                        $base_storage_type = $storage_type;
                    }
                }
            }

            foreach (@members) {

                # all equivalenced variables will be made type 'save'
                my $save = 1;
                if ( $_ ne $base_member ) {
                    my $offset = $relative_offset{$_} - $min_offset;

                    $local_symbol_table->set(
                        'name'                     => $_,
                        'local_equivalence_name'   => $base_member,
                        'local_equivalence_offset' => $offset,
                        'save'                     => $save,
                    );

                    # update the index string for an array
                    my $storage_type =
                      $local_symbol_table->get( $_, 'storage_type' );
                    if ( $storage_type eq '@' && $offset ne '0' ) {
                        my $index_string =
                          $local_symbol_table->get( $_, 'index_string' );
                        $index_string = "$offset + $index_string";
                        $local_symbol_table->set(
                            'name'         => $_,
                            'index_string' => $index_string
                        );
                    }

                    # be sure that a scalar base has all scalar members
                    # shouldn't happen, because the array would have been
                    # selected.
                    if ( $base_storage_type eq '$' && $storage_type ne '$' ) {
                        error(
"Program bug making equivalance of $_ to $base_member\n"
                        );
                    }
                }
                else {
                    $local_symbol_table->set( 'name' => $_, 'save' => $save, );
                }
            }

            ## DEBUG INFO
##            print "DEBUG: group $i has members: (@members)\n";
##            print "DEBUG: the minimum offset member is $base_member with type $base_storage_type\n";
##            foreach (@members) {
##                $relative_offset{$_} = $relative_offset{$_} - $min_offset;
##                print "DEBUG: group $i member $_ offset $relative_offset{$_}\n";
##            }
        }
    }

    sub eval_equivalence_index {
        my ( $name, $rindex ) = @_;
        my $index_value = undef;
        if ( !defined($rindex) ) {
            $index_value = 0;
            return $index_value;
        }
        my $type = $local_symbol_table->get( $name, 'storage_type' );
        if ( !$type ) {
            error("Program error: $name is not in the symbol table\n");
            return $index_value;
        }
        elsif ( $type eq '$' ) {
            if ( @$rindex > 0 ) {
                error("$name is scalar but is subscripted here\n");
                return $index_value;
            }
        }
        elsif ( $type eq '@' ) {
            my $rimaxs = $local_symbol_table->get( $name, 'imax_values' );
            my $rimins = $local_symbol_table->get( $name, 'imin_values' );
            if ( !$rimaxs ) {
                error("array $name has not been dimensioned\n");
                return $index_value;
            }
            if ( !$rimins ) {
                error("array $name has not been dimensioned\n");
                return $index_value;
            }
            elsif ( @$rimaxs != @$rimins ) {
                error("array $name has not been dimensioned properly\n");
                return $index_value;
            }

            if ( @$rimaxs == 0 ) {
                $index_value = 0;
            }
            elsif ( @$rimaxs != @$rindex ) {
                my $old_count = @$rimaxs;
                my $new_count = @$rindex;
                error(
"Equivalence expecting $old_count subscripts for $name but found $new_count\n"
                );
                return $index_value;
            }
            else {
                my $n    = @$rimaxs - 1;
                my $imin = $rimins->[$n];
                my $imax = $rimaxs->[$n];
                $index_value = $rindex->[$n] - $imin;

                # We're evaluating something like this:
                # $_[0]-1+$idim * ($_[1]-1+$jdim * ($_[2]-1+$kdim * ($_[3]-1)))
                for ( my $i = $n - 1 ; $i >= 0 ; $i-- ) {
                    $imax = $rimaxs->[$i];
                    $imin = $rimins->[$i];
                    my $ix = $rindex->[$i];
                    $index_value =
                      ( $ix - $imin ) + ( $imax - $imin + 1 ) * $index_value;
##                    print
##"DEBUG: index of $name with $imin:$imax is $index_value\n";
                }
            }
        }
        else {
            error(
"$name is equivalenced and must be scalar or array but has type '$type'\n"
            );
            return $index_value;
        }
        ##print "DEBUG: returning $index_value for $name\n";
        return $index_value;
    }

    sub make_common_equivalence {

        my ( $lname, $lindex, $lindex_value, $rname, $rindex, $rindex_value ) =
          @_;
        my $common_name    = $local_symbol_table->get( $rname, 'common_name' );
        my $is_master_copy = !defined( $common_symbol_tables{$common_name} );
        my $package_name   = $common_name;
        my $offset         =
          $local_symbol_table->get( $rname, 'common_offset' ) + $rindex_value -
          $lindex_value;

        if ( $local_symbol_table->get( $lname, 'is_common' ) ) {
            my $old_offset =
              $local_symbol_table->get( $lname, 'common_offset' );
            ##print "DEBUG: testing $lname->$rname old=$old_offset new=$offset\n";
            if ( $old_offset != $offset ) {

                ## FIXME: need better diagnostic output here
                print STDERR "Inconsistent equivalences for $lname\n";
                return;
            }
        }

        if ( $offset < 0 ) {
            error(
"attempting to extend common backwards with equivalence of $lname to $rname\n"
            );
            return;
        }

        # if this is not the master copy of the common block, then
        # the offset must match the master copy of this variable
        unless ($is_master_copy) {
            my $master_offset =
              $common_symbol_tables{$common_name}
              ->get( $lname, 'common_offset' );

            ##print "DEBUG: $lname to $rname : master offset=$master_offset offset=$offset\n";
            unless ( defined($master_offset) && $master_offset == $offset ) {

                $package_name = "";
                push @{ $rlocal_common_equivalences{$common_name} }, $lname;
            }
        }

        ###############################################################
        ## FIXME: the equivalences are values now and should be symbolic
        ## BUT WATCH OUT FOR DEPENDENCIES OF PARAMETERS
        ###############################################################
        $local_symbol_table->set(
            'name'                => $lname,
            'is_common'           => 1,
            'common_name'         => $common_name,
            'common_offset'       => $offset,
            'common_package_name' => $package_name,
            'equivalence_to'      => $rname,
            'equivalence_lindex'  => $lindex,
            'equivalence_rindex'  => $rindex,
        );
    }

    sub compute_array_size {

        my ( $rimin_values, $rimax_values ) = @_;
        my $array_size = 1;
        if ( $rimin_values && $rimax_values ) {
            foreach my $i ( 0 .. @$rimax_values - 1 ) {
                my $imin = $rimin_values->[$i];
                my $imax = $rimax_values->[$i];
                if ( defined($imin) && defined($imax) ) {
                    $array_size *= ( $imax - $imin + 1 );
                }
            }
        }
        return $array_size;
    }

    sub scan_dimensions {

        # get and store the dimensions of any arrays
        my $max_token_index = @$rtokens - 1;
        my $depth           = 0;
        my $i_start         = 0;
        if ($stmt_type) { $i_start++ }
        for my $i ( $i_start .. $max_token_index ) {
            my $type  = $rtoken_types->[$i];
            my $token = $rtokens->[$i];
            if ( $type eq '(' ) { $depth++; next }
            if ( $type eq ')' ) { $depth--; next }
            if (   $type eq '@'
                && $i < $max_token_index
                && $rtoken_types->[ $i + 1 ] eq '('
                && $depth == 0 )
            {
                my $i_comma = $i + 1;
                my @dims;
                my @imins;
                my @imaxs;
                my @imin_values;
                my @imax_values;
                my @dependencies;
                while (1) {
                    my $i_last = $i_comma + 1;
                    $i_comma = find_next_comma($i_last);
                    if ( $i_comma < $i_last + 1 ) {
                        error("dimension error for $token\n");
                        last;
                    }
                    my ( $imin, $imax, $imin_value, $imax_value, $dim,
                        $rdependencies )
                      = split_colon_dimensions( $i_last, $i_comma - 1 );
                    push @dims,        $dim;
                    push @imins,       $imin;
                    push @imaxs,       $imax;
                    push @imin_values, $imin_value;
                    push @imax_values, $imax_value;
                    if ($rdependencies) {
                        push @dependencies, @$rdependencies;
                    }
                    last if ( $rtokens->[$i_comma] ne ',' );
                }
                my $index_string = make_index_string( \@dims, \@imins );
                my $array_size =
                  compute_array_size( \@imin_values, \@imax_values );
                $local_symbol_table->set(
                    'name'         => $token,
                    'imins'        => \@imins,
                    'imaxs'        => \@imaxs,
                    'imin_values'  => \@imin_values,
                    'imax_values'  => \@imax_values,
                    'array_size'   => $array_size,
                    'dependencies' => \@dependencies,
                    'index_string' => $index_string
                );
                ##print "Dims for $token are (@dims)\n";
            }
        }
    }

    sub process_call_args {

        # setup for call by reference
        my $token;
        my $type;
        my $last_token      = " ";
        my $last_token_type = " b ";
        my $next_token      = $rtokens->[0];
        my $next_token_type = $rtoken_types->[0];
        my @paren_type      = ('B');
        my @new_tokens;
        my @new_token_types;
        my $depth                = 0;
        my @insert_tokens_before = (undef);
        my $max_token_index      = @$rtokens - 1;
        my $i_begin              = 0;

        ## TESTING: The items in an iolist are treated as if they were a call list
        ##if ($stmt_type eq 'write') {$paren_type[0]='x';}
        ##if ($stmt_type eq 'print') {$paren_type[0]='x'; $i_begin=2}

        for my $i ( $i_begin .. $max_token_index ) {
            $token = $next_token;
            $type  = $next_token_type;
            if ( $i < $max_token_index ) {
                $next_token      = $rtokens->[ $i + 1 ];
                $next_token_type = $rtoken_types->[ $i + 1 ];
            }
            else {
                $next_token      = " ";
                $next_token_type = 'b';
            }

            # NOTE: Checking token, not type, to avoid problems with
            # parens converted to square brackets. (callarg.f)
            if ( $token eq ')' ) {
                $depth--;
                next;
            }
            if ( $token eq '(' ) {
                $depth++;
                $paren_type[$depth] = $last_token_type;
                next;
            }

            # check for arg after '(' and ','
            ## Note: and also after ')' to process the iolist of a write stmt
            # CAUTION: The opposite of this statement must exist in
            # scan_bareword
            if ( $paren_type[$depth] eq 'x' && $last_token_type =~ /^[\(\,]$/ )
            {

                # check for alternate return labels
                if (   $type eq '*'
                    && $next_token_type eq 'd'
                    && $stmt_type       eq 'call' )
                {
                    my $label = get_goto_label($next_token);
                    push @alternate_return_labels, $label;
                    $token           = "'$label'";
                    $type            = 'q';
                    $next_token      = "";
                    $next_token_type = 'b';
                }

                # prepend an '&' if passing an external routine
                if ( $type eq 'x' && $next_token_type =~ /^[\,\)]$/ ) {
                    $token = '&' . $token;
                    ##error("DEBUG: passing external $token here\n");
                }

                # Decide on call method (by reference or by value or as
                # array)
                my $call_by = 'reference';
                if ( $paren_type[$depth] eq '$x' ) { $call_by = 'value' }
                if ( $type eq 'k' && $token eq '%val' ) {
                    $token   = "";
                    $type    = 'b';
                    $call_by = 'value';
                }
                if ( $type eq 'k' && $token eq '%ref' ) {
                    $token   = "";
                    $type    = 'b';
                    $call_by = 'reference';
                }

                # decide if this is a simple term or an expression
                my $is_expression = 0;
                my $i_term_end    = $i;
                if ( $next_token_type eq '(' ) {
                    $i_term_end = find_closing_paren( $i + 1 );
                }
                if ( $rtokens->[ $i_term_end + 1 ] !~ /^[\,\)]$/ ) {
                    $is_expression = 1;
                }

                # Take care of an expression
                if ($is_expression) {

                    if ( $call_by eq 'reference' ) {
                        push @new_tokens,      '\\';
                        push @new_token_types, '\\';

                        # parenthesize an expression in the arg list so that we
                        # can get its reference
                        push @new_tokens,      '(';
                        push @new_token_types, '(';
                        my $i_comma = find_next_comma($i);
                        unless ( $insert_tokens_before[$i_comma] ) {
                            $insert_tokens_before[$i_comma] = "";
                        }
                        $insert_tokens_before[$i_comma] =
                          ')' . $insert_tokens_before[$i_comma];
                    }

                    # we still have to decorate any array even if this is an
                    # expression.  it would be nicer to do this in scan_bare();
                    next unless $type eq '@';
                }

                # Handle an array
                if ( $type eq '@' ) {

                    # Handle subscripted array in call parameter list, such as
                    #    call some_sub(ka(3), ...)
                    if ( $next_token_type eq '(' ) {

                        if ( $call_by eq 'value' || $is_expression ) {
                            if ( use_simple_access($token) ) {

                                $next_token_type = '[';
                                $next_token      = '[';

                                my $rimin_values =
                                  $local_symbol_table->get( $token,
                                    'imin_values' );
                                my $imin            = $rimin_values->[0];
                                my $i_closing_paren =
                                  find_closing_paren( $i + 1 );

                                ## FIXME: split into multiple tokens
                                ## to avoid future coding problems
                                ## i.e. '-' '1' ']'
                                ## see similar coding in scan bareword and
                                ## combine if possible
                                my $closing_tok = "]";
                                if ( $imin != 0 ) { $closing_tok = "- $imin]"; }

                                # evaluate the index if numeric
                                if (   $i_closing_paren eq $i + 3
                                    && $rtoken_types->[ $i + 2 ] eq 'd' )
                                {
                                    $rtokens->[ $i + 2 ] -= $imin;
                                    $closing_tok = ']';
                                }

                                $rtokens->[$i_closing_paren] = $closing_tok;
                                $token = '$' . $token;
                            }
                            else {
                                $token = "\$$token->";
                            }
                        }

                        # use simplest method for parameter of (1)
                        elsif ($i + 3 <= $max_token_index
                            && join_tokens( $i + 1, $i + 3 ) =~ /^\(1\)$/
                            && !$local_symbol_table->get( $token, 'is_common' )
                          )
                        {
                            if ( $is_arg{$token} ) {
                                $token = "\$R_$token";
                            }
                            else {
                                $token = "[\\\@$token,0]";
                            }

                            # Remove the '(1)'
                            $rtokens->[ $i + 1 ] = $rtokens->[ $i + 2 ] =
                              $rtokens->[ $i + 3 ] = $next_token = "";

                            $rtoken_types->[ $i + 1 ]   =
                              $rtoken_types->[ $i + 2 ] =
                              $rtoken_types->[ $i + 3 ] = $next_token_type = "";
                        }

                        # Otherwise, use the sub call method
                        #    call some_sub(ka(3), ...)
                        # becomes
                        #    some_sub($C_ka->(3), ...)
                        else {
                            $local_symbol_table->set(
                                'name'          => $token,
                                'need_call_sub' => 1
                            );
                            $token = "\$C_$token->";
                            $token = rename_common_token($token);
                        }
                    }

                    # Handle non-subscripted array, such as
                    #    call some_sub(ka, ...)
                    # if ka is a local array:
                    #    some_sub([\@ka,0], ...)
                    # if ka comes through as a parameter:
                    #    some_sub($R_ka, ...)
                    else {
                        my $bareword = $token;
                        if ( $is_arg{$bareword}
                            && ( $next_token_type =~ /^[\)\,]$/ ) )
                        {
                            $token = "\$R_$bareword";
                        }
                        elsif (
                            $local_symbol_table->get( $bareword, 'is_common' ) )
                        {
                            $local_symbol_table->set(
                                'name'          => $token,
                                'need_call_sub' => 1
                            );
                            $token = "\$C_$token->()";
                            $token = rename_common_token($token);
                        }
                        else {
                            $token = "[\\\@$bareword,0]";
                        }
                    }
                }

                # function call
                elsif ( $type =~ /x$/ ) {
                    if ( $call_by eq 'reference' ) {
                        push @new_tokens,      '\\';
                        push @new_token_types, '\\';
                    }
                }

                # scalar or constant
                elsif ( $call_by eq 'reference' ) {
                    if ( $type eq '$' ) {

                        # Pass the $R_bareword form if
                        # it is an arg followed by a comma or )
                        # This will prevent trouble in case it is really
                        # an undeclared array
                        my $bareword = $token;
                        $bareword =~ s/^\$+//;
                        if ( $is_arg{$bareword}
                            && ( $next_token_type =~ /^[\)\,]$/ ) )
                        {
                            $token = "\$R_$bareword";
                            next;
                        }
                        else {
                            push @new_tokens,      '\\';
                            push @new_token_types, '\\';
                        }
                    }
                    else {
                        push @new_tokens,      '\\';
                        push @new_token_types, '\\';
                    }
                }
            }
        }
        continue {
            if ( $insert_tokens_before[$i] ) {
                push @new_tokens,      $insert_tokens_before[$i];
                push @new_token_types, $insert_tokens_before[$i];
            }
            push @new_tokens,      $token;
            push @new_token_types, $type;
            $last_token      = $token;
            $last_token_type = $type;
        }
        $rtokens      = \@new_tokens;
        $rtoken_types = \@new_token_types;
    }

    sub process_character_subscripts {

        # Convert all character subscripts in a line to use substr.
        my ($colon_count)   = @_;
        my $max_token_index = @$rtokens - 1;
        my $depth           = 0;
        my $i_closing_paren = @$rtokens - 1;
        my @i_opening;
        my $colons_found = 0;
        my $more_to_do   = 1;

        # keep rescanning until we see all of the colons or hit an error

        # Note that these may be nested, as in this example:
        #   state(itemp:itemp+1+kartoi(token(1:1)))='h'
        # This is handled by rescanning from the start of the line
        # until we have see all of the colons.
        while ($more_to_do) {
            $more_to_do = 0;
            for my $i ( 1 .. @$rtokens - 1 ) {
                my $type  = $rtoken_types->[$i];
                my $token = $rtokens->[$i];
                if ( $type eq '(' ) {
                    $depth++;
                    $i_opening[$depth] = $i;
                    next;
                }
                if ( $type eq ')' ) { $depth--; next }
                if ( $type eq ':' ) {

                    ++$colons_found;
                    if ( $depth == 0 || !defined( $i_opening[$depth] ) ) {
                        error("Unexpected ':'");
                    }
                    else {
                        if ( fix_character_subscript( $i_opening[$depth], $i ) )
                        {
                            $more_to_do = ( $colons_found < $colon_count );
                        }
                    }
                    last;
                }
            }
        }
    }

    sub fix_character_subscript {

        # fix the subscripted character variable whose colon is at index
        # $i_colon to use the perl substr function
        #
        # return undef if not successful
        # return 1 if successful
        #
        #  Example:
        #  INPUT TOKENS:
        #     $c = $namev($lc:$lc+4)
        #  OUTPUT TOKENS:
        #     $c = substr($namev,$lc-1,5);

        my ( $i_opening_paren, $i_colon ) = @_;

        # we are given the opening paren and colon, and have to go find
        # the closing paren:
        my $i_closing_paren = find_closing_paren($i_opening_paren);

        # Shouldn't happen..
        # Unbalanced lines should not get here, but just just checking..
        if ( $rtokens->[$i_closing_paren] ne ')' ) {
            error(
"Program error fixing character variable: cannot find closing paren\n"
            );
            return undef;
        }

        # This shouldn't happen:
        if ( $i_opening_paren <= 0 ) {
            error(
"Program error fixing character variable: no character variable\n"
            );
            return undef;
        }

        # The token before the opening paren may be the name of the
        # character variable.
        #     $c = $namev($lc:$lc+4)
        #               ^
        #               |
        #             $i_variable
        my $i_variable = $i_opening_paren - 1;

        # But we have to walk back to find the start of an array of character
        # data.  For example, we have to go back to find 'namegv' in the
        # following line:
        #   call ch2a4(namegv(l)(lc:lc+4),ibuff(li),1)
        #                      ^
        #                      |
        #                    $i_variable
        #
        # Note that we may be looking at a ']' if a local array has
        # already been converted to a perl array.
        # Also not that tokens might be '-]' because of coding in
        # process_call_args. Thus, the following line  (char5.f)
        #    ival = ctoi(result(i)(1:lstr), ii)
        # would have the intermediate form, where the single token is
        # shown:
        #    $ival=int(ctoi($result[$i-1](1:lstr),$ii));
        #                             ---
        my $is_array_ref = 0;
        if ( $rtokens->[$i_variable] =~ /[\)\]]$/ ) {

            $is_array_ref = 1;
            my $i_open = find_opening_paren($i_variable);
            $i_variable = $i_open - 1;
        }
        ##print "DEBUG: $rtokens->[$i_variable] is=$is_array_ref  iopen=$i_opening_paren iv=$i_variable\n";

        # if there is no '$', it could be because this is a character
        # array within a call statement, which is ok.  Here is an example:
        #
        #      dimension ibuff(1)
        #      character*(*) namegv(1)
        #      call ch2a4(namegv(l)(lc:lc+4),ibuff(li),1)
        #
        # The array namegv will not be decorated yet because it is
        # a call parameter.  The existance of the character subscripts
        # now tells us that a scalar value is being passed, rather than
        # an array reference, so we can decorate it with a '$'.
        if ( $rtokens->[$i_variable] !~ /^\$/ ) {
            my $bareword = $rtokens->[$i_variable];
            if (   $is_array_ref
                && $local_symbol_table->get( $bareword, 'storage_type' ) eq
                '@' )
            {
                $rtokens->[$i_variable] = '$' . $bareword;
            }
            else {
                error(
                    "Unexpected ':' found -- not within a character variable\n"
                );
                return undef;
            }
        }

        # get setup to build a new line of tokens...
        my @new_tokens;
        my @new_token_types;

        my $add_old_tokens = sub {
            my ( $i1, $i2 ) = @_;
            push @new_tokens,      @$rtokens[ $i1 .. $i2 ];
            push @new_token_types, @$rtoken_types[ $i1 .. $i2 ];
        };

        my $add_new_token = sub {
            my ( $token, $type ) = @_;
            push @new_tokens,      $token;
            push @new_token_types, $type;
        };

        # copy the old leading tokens plus the new 'substr' stuff
        $add_old_tokens->( 0,       $i_variable - 1 );
        $add_new_token->( 'substr', 'k' );
        $add_new_token->( '(',      '(' );
        $add_old_tokens->( $i_variable, $i_opening_paren - 1 );
        $add_new_token->( ',', ',' );

        # find the next arg for substr: the starting string position
        my $is_constant_pos = 1;
        my $pos             = 0;
        if ( $i_colon > $i_opening_paren + 1 ) {
            my $pos_str;
            my $terminal_plus_one =
              (      $rtokens->[ $i_colon - 1 ] eq '1'
                  && $rtokens->[ $i_colon - 2 ] eq '+' );
            if ($terminal_plus_one) {
                $pos_str = join_tokens( $i_opening_paren + 1, $i_colon - 3 );
            }
            else {
                $pos_str =
                  join_tokens( $i_opening_paren + 1, $i_colon - 1 ) . "-1";
            }
            if ( $pos_str =~ /^[\+\-\d]+$/ ) {
                $pos = eval($pos_str);
            }
            else {
                if ($terminal_plus_one) {
                    $add_old_tokens->( $i_opening_paren + 1, $i_colon - 3 );
                }
                else {
                    $add_old_tokens->( $i_opening_paren + 1, $i_colon - 1 );
                    $add_new_token->( '-', '-' );
                    $add_new_token->( '1', '1' );
                }
                $is_constant_pos = 0;
            }
        }
        if ($is_constant_pos) {
            $add_new_token->( $pos, 'd' );
        }

        # find the next arg for substr: the character count
        # we want to keep this as readable as possible, so there are
        # a lot of cases, depending on whether the two fortran indexes
        # were constants or symbolic values
        if ( $i_closing_paren > $i_colon + 1 ) {
            if ($is_constant_pos) {
                my $end_str = join_tokens( $i_colon + 1, $i_closing_paren - 1 );

                # constant-constant
                if ( $end_str =~ /^[\+\-\d]+$/ ) {
                    my $count = eval( $end_str - $pos );
                    $add_new_token->( ',', ',' );
                    $add_new_token->( $count, 'd' );
                }

                # constant-symbolic
                else {
                    $add_new_token->( ',', ',' );
                    $add_old_tokens->( $i_colon + 1, $i_closing_paren - 1 );
                    if ( $pos > 0 ) {
                        $add_new_token->( '-', 'd' );
                        $add_new_token->( $pos, 'd' );
                    }
                }
            }

            # symbolic-either
            else {

                $add_new_token->( ',', ',' );

                # could be something like (K:K+1), so remove identical terms
                # in search of a constant count
                my $ii = $i_opening_paren + 1;
                my $jj = $i_colon + 1;
                while ( $ii < $i_colon && $jj < $i_closing_paren ) {
                    last if ( $rtokens->[$ii] ne $rtokens->[$jj] );
                    $ii++;
                    $jj++;
                }

                my $eval_str = "1";
                if ( $jj < $i_closing_paren ) {

                    # see if a '+' is needed: we may have either stopped before
                    # or after an operator was expected.
                    if ( $rtokens->[$jj] !~ /^[\+\-\*\/]$/ ) {
                        $eval_str .= ' + ';
                    }
                    $eval_str .= join_tokens( $jj, $i_closing_paren - 1 );
                }

                my $str1 = "";
                if ( $ii < $i_colon ) {
                    $eval_str .=
                      "-" . '(' . join_tokens( $ii, $i_colon - 1 ) . ')';
                }

                # constant count
                if ( $eval_str =~ /^[\s\+\-\d\)\(]+$/ ) {
                    my $const = eval($eval_str);
                    $add_new_token->( $const, 'd' );
                }

                # symbolic count
                else {
                    $add_old_tokens->( $jj, $i_closing_paren - 1 );
                    $add_new_token->( '-', '-' );
                    $add_new_token->( '(', '(' ) if ( $i_colon - 1 > $ii );
                    $add_old_tokens->( $ii, $i_colon - 1 );
                    $add_new_token->( ')', ')' ) if ( $i_colon - 1 > $ii );

        # TODO: see if this might be combined with a constant at i_closing_paren
                    $add_new_token->( '+', '+' );
                    $add_new_token->( '1', 'd' );
                }

            }
        }

        # success: now finish off the line by copying the remaining old tokens
        $add_old_tokens->( $i_closing_paren, @$rtokens - 1 );
        $rtokens      = \@new_tokens;
        $rtoken_types = \@new_token_types;
        return 1;
    }

    sub find_closing_paren {

        # returns index of the next ')' at a depth less than or
        # equal to the depth of the starting token
        my ($i_start)       = @_;
        my $starting_depth  = 0;
        my $depth           = 0;
        my $max_token_index = @$rtoken_types - 1;
        my $type;
        for ( my $i = $i_start ; $i <= $max_token_index ; $i++ ) {
            $type = $rtoken_types->[$i];
            if ( $type eq ')' ) {
                $depth--;
                return $i if $depth <= $starting_depth;
            }
            elsif ( $type eq '(' ) {
                $depth++;
            }
        }

        # unbalanced line; shouldn't happen because this
        # statement should have been made a coment; probably an error
        ##my $line=join '',@$rtokens[0..$max_token_index];
        ##print STDERR "$line\n";
        print STDERR " Program Bug in find_closing_paren: unbalanced \n ";
        return undef;
    }

    sub find_opening_paren {

        # returns index of the previous '(' or '[' at a depth less
        # than or equal to the depth of the starting token
        my ($i_start)       = @_;
        my $starting_depth  = 0;
        my $depth           = 0;
        my $max_token_index = @$rtoken_types - 1;
        if ( $i_start > $max_token_index ) {
            print STDERR
              "Program error in find_open_paren: $i_start > $max_token_index\n";
        }
        my $type;
        for ( my $i = $i_start ; $i >= 0 ; $i-- ) {
            $type = $rtoken_types->[$i];
            if ( $type eq ')' || $type eq ']' ) {
                $depth--;
            }
            elsif ( $type eq '(' || $type eq '[' ) {
                $depth++;
                return $i if $depth >= $starting_depth;
            }
        }

        # unbalanced line; shouldn't happen because this
        # statement should have been made a coment; probably an error
        print STDERR " Program bug in find_opening_paren: unbalanced \n ";
        return undef;
    }

    sub find_next_comma {

        # Find index of next comma or ')' at the level of the first
        # character after the starting token.
        # The token to find may be changed from comma to something else
        my ( $i_start, $token_to_find ) = @_;
        unless ($token_to_find) { $token_to_find = ',' }
        my $i_comma;
        my $starting_depth  = 0;
        my $depth           = 0;
        my $max_token_index = @$rtoken_types - 1;
        my $i;
        my $type = "";

        for ( $i = $i_start ; $i <= $max_token_index ; $i++ ) {
            $type = $rtoken_types->[$i];
            if ( $type eq ')' ) {
                $depth--;
                if ( $depth < $starting_depth ) {
                    return wantarray ? ( $i, $type ) : $i;
                }
            }
            elsif ( $type eq '(' ) {
                $depth++;
            }
            elsif ( $type eq $token_to_find ) {
                if ( $i >= $i_start && $depth == $starting_depth ) {
                    return wantarray ? ( $i, $type ) : $i;
                }
            }
        }
        return wantarray ? ( $max_token_index, "" ) : $max_token_index;
    }

    sub split_commas {

        # split current token list on commas at the highest level
        # returns all terms
        my ( $i_start, $i_end ) = @_;
        my @terms;
        my $max_token_index = @$rtoken_types - 1;
        if ( !defined($i_end) ) { $i_end = $max_token_index }
        if ( $i_start <= $i_end ) {
            my $starting_depth = 0;
            my $depth          = 0;
            my $i;
            if ( $i_end > $max_token_index ) { $i_end = $max_token_index }
            my $i_begin = $i_start;
            if ( $rtokens->[$i_begin] =~ /^[\,\(]$/ ) { $i_begin++ }

            my $type;
            for ( $i = $i_start + 1 ; $i <= $i_end ; $i++ ) {
                $type = $rtoken_types->[$i];
                if ( $type eq ')' ) {
                    $depth--;
                    if ( $depth < $starting_depth ) {
                        last;
                    }
                }
                elsif ( $type eq '(' ) {
                    $depth++;
                }
                elsif ( $type eq ',' ) {
                    if ( $depth == $starting_depth ) {
                        my $term = join_tokens( $i_begin, $i - 1 );
                        push @terms, $term;
                        $i_begin = $i + 1;
                    }
                }
                if ( $i == $i_start ) { $starting_depth = $depth }
            }
            if ( $i > $i_begin ) {
                my $term = join_tokens( $i_begin, $i - 1 );
                push @terms, $term;
            }
        }
        return ( wantarray ? @terms : \@terms );
    }

    sub find_commas {

        # Find indexes of all commas at the level of the first
        # character after the starting token.
        #
        # Returns:
        # the ending index (a ')' or $max_token_index),
        # and a reference to the list of comma index

        my ( $i_start, $i_end ) = @_;
        my $max_token_index = @$rtoken_types - 1;
        if ( !defined($i_end) ) { $i_end = $max_token_index }
        my @i_commas;
        my $starting_depth = 0;
        my $depth          = 0;
        my $i;
        my $type;

        for ( $i = $i_start ; $i <= $i_end ; $i++ ) {
            $type = $rtoken_types->[$i];
            if ( $type =~ /[\)\]]$/ ) {
                $depth--;
                last if $depth < $starting_depth;
            }
            elsif ( $type =~ /^[\(\[]$/ ) {
                $depth++;
            }
            elsif ( $type eq ',' ) {
                if ( $depth == $starting_depth ) { push @i_commas, $i }
            }
        }
        return ( wantarray ? @i_commas : \@i_commas );
    }

    {    # hardwired token translations

        # change $token and $type as necessary
        my ( $i_tok, $token, $type );
        my $conversion_code = {

            # TODO: some of these need to know types on either side
            # so may need to do this late
            'w' => sub {
                $token =~ s/\$/_DOLLAR_/g;
            },
            'Q' => sub {
                if ( $stmt_type eq 'format' ) {

                   # quotes within format statements will be enclosed in ' marks
                   # all existing ' marks will be escaped
                    $token =~ s/'/\\'/g;
                    $token = "'$token'";
                }
                else {
                    $token = quote_string($token);
                }
            },
            'B' => sub {
                $token = '0b' . substr( $token, 2, -1 );
            },
            'O' => sub {
                $token = substr( $token, 2, -1 );
                if ( $token !~ /^0/ ) { $token = '0' . $token }
            },
            'Z' => sub {
                $token = '0x' . substr( $token, 2, -1 );
            },
            'd' => sub {

                # all numbers translate into whatever precision Perl was
                # compiled with
                $token =~ s/[dDEQq]/e/;

                # avoid interpretation of number with leading 0's
                # as an octal value.  For example:
                # 01e0 is ok in fortran but fails in in perl
                if ( $token =~ /^0+(\d.*)$/ ) { $token = $1; }
            },
            '//' => sub {
                if ( $stmt_type ne 'format' ) {
                    $token = '.';
                    $type  = $token;
                }
            },
            '.ne.' => sub {
                $token = '!=';
                $type  = $token;
            },
            '.eq.' => sub {
                $token = '==';
                $type  = $token;
            },
            '.gt.' => sub {
                $token = '>';
                $type  = $token;
            },
            '.ge.' => sub {
                $token = '>=';
                $type  = $token;
            },
            '.le.' => sub {
                $token = '<=';
                $type  = $token;
            },
            '.lt.' => sub {
                $token = '<';
                $type  = $token;
            },
            '.or.' => sub {
                $token = '||';
                $type  = $token;
            },
            '.and.' => sub {
                $token = '&&';
                $type  = $token;
            },
            '.not.' => sub {
                $token = '!';
                $type  = $token;
            },
            '.true.' => sub {
                $token = '1';
                $type  = 'd';
            },
            '.false.' => sub {
                $token = '0';
                $type  = 'd';
            },

            # TODO: these should be translated into calls to the subs
            # need to locate the bounds of the 2 args and call sub eqv
            '.eqv.' => sub {
                print STDERR ".eqv. not implement yet\n";
            },
            '.neqv.' => sub {
                print STDERR ".neqv. not implement yet\n";
            },
        };

        sub translate_tokens_to_perl {

            # first do any hardwired rules
            my $max_token_index = @$rtoken_types - 1;
            for ( $i_tok = 0 ; $i_tok <= $max_token_index ; $i_tok++ ) {
                $token = $rtokens->[$i_tok];
                $type  = $rtoken_types->[$i_tok];
                my $code = $conversion_code->{$type};
                if ($code) {
                    $code->();
                    $rtokens->[$i_tok]      = $token;
                    $rtoken_types->[$i_tok] = $type;
                    next;
                }
            }

            # finish up
            scan_barewords();
        }
    }
}

sub simplify_list {

    # Given a sorted list of integers,
    # Create a string which encodes the list with the '..' notation
    # wherever possible.  Example:
    # (1,2,3,5,6,7,9) should become a string '(1..3, 5..7, 9,)'
    my ($rlist) = @_;
    my @newlist;
    my $in_run = 0;
    my $n      = $rlist->[0];
    my $n_last = $n;
    my $n_beg;
    my $imax = @$rlist - 1;

    foreach my $i ( 1 .. $imax ) {
        $n = $rlist->[$i];
        if ($in_run) {
            next if ( $n == $n_last + 1 );
            if ( $n_last > 1 + $n_beg ) {
                push @newlist, "$n_beg .. $n_last,";
            }
            else {
                push @newlist, "$n_beg,";
                push @newlist, "$n_last,";
            }
            $in_run = 0;
        }
        else {
            if ( $n == $n_last + 1 ) {
                $in_run = 1;
                $n_beg  = $n_last;
            }
            else {
                push @newlist, "$n_last,";
            }
        }
    }
    continue { $n_last = $n }
    if ($in_run) {
        if ( $n_last > 1 + $n_beg ) {
            push @newlist, "$n_beg .. $n_last,";
        }
        else {
            push @newlist, "$n_beg,";
            push @newlist, "$n_last,";
        }
    }
    else {
        push @newlist, "$n_last,";
    }
    return join " ", @newlist;
}

##package Fortran::F77toPerl::Output; ??
{    # beginning of output routines

    my %is_label;
    my %is_target;
    my %output_lines;
    my @pending_comments;
    my $pending_side_comment = "";
    my $outer_block_name;
    my $i_first_executable_statement;

    sub clear_output {
        %is_label                     = ();
        %is_target                    = ();
        %output_lines                 = ();
        @pending_comments             = ();
        $pending_side_comment         = "";
        $outer_block_name             = "";
        $i_first_executable_statement = undef;
    }

    my %is_valid_output_type;

    BEGIN {
        @_ =
          qw(comment constant side_comment save data
          parameter_declaration parameter_data subroutine
          args entry use_vars common declaration format code);
        @is_valid_output_type{@_} = (1) x scalar(@_);
    }

    sub mark_first_executable_statement {

        # Remember the index of the first executable statement
        # This is not currently needed, and may be removed at a later date
        unless ( defined($i_first_executable_statement) ) {
            $i_first_executable_statement = 0;
            if ( $output_lines{'code'} ) {
                $i_first_executable_statement =
                  scalar @{ $output_lines{'code'} };
            }
        }
    }

    sub write_output_line {
        my ( $type, $line ) = @_;

        unless ( $is_valid_output_type{$type} ) {
            print STDERR
              " Program bug: write_output_line got invalid type: $type \n ";
            my @keys = keys %is_valid_output_type;
            print STDERR " expecting one of: = @keys \n ";
            $type = 'code';
        }

        # tentatively store comments and output them ahead of the
        # next non-comment type
        if ( $type eq 'comment' ) {
            push @pending_comments, $line . " \n ";
        }
        elsif ( $type eq 'side_comment' ) {
            if ($pending_side_comment) {
                push @pending_comments, $line;
            }
            else {
                $pending_side_comment = $line;
            }
        }
        else {

            if (   $pending_side_comment
                && $type ne 'label' )
            {
                $line .= ' ' . $pending_side_comment;
            }
            $line .= "\n";
            push @{ $output_lines{$type} }, @pending_comments, $line;
            @pending_comments     = ();
            $pending_side_comment = "";
        }
    }

    sub write_outer_block_name {
        $outer_block_name = '# ' . $_[0];
    }

    sub write_side_comment {
        write_output_line( 'side_comment', $_[0] );
    }

    sub write_constant {
        write_output_line( 'constant', $_[0] );
    }

    sub write_comment {
        write_output_line( 'comment', $_[0] );
    }

    sub write_label {
        write_output_line( 'code', $_[0] );
        $is_label{ $_[0] } = 1;
    }

    sub mark_target {
        $is_target{ $_[0] }++;
    }

    sub write_code {
        write_output_line( 'code', $_[0] ) if $_[0];
    }

    sub write_common {
        write_output_line( 'common', $_[0] ) if $_[0];
    }

    sub write_use_vars {
        write_output_line( 'use_vars', $_[0] ) if $_[0];
    }

    sub write_args {
        write_output_line( 'args', $_[0] ) if $_[0];
    }

    sub write_subroutine {
        write_output_line( 'subroutine', $_[0] );
    }

    sub write_format {
        write_output_line( 'format', $_[0] );
    }

    sub write_save {
        write_output_line( 'save', $_[0] );
    }

    sub write_data {
        write_output_line( 'data', $_[0] );
    }

    sub write_parameter_declaration {
        write_output_line( 'parameter_declaration', $_[0] );
    }

    sub write_parameter_data {
        write_output_line( 'parameter_data', $_[0] );
    }

    sub write_declaration {
        write_output_line( 'declaration', $_[0] );
    }

    sub get_perl_function {

        # retrieve all of the code that has been stored for this
        # program unit, in the proper order
        my @output;

        # remove non-target labels
        my @new_code;
        my $last_output_line_is_label = 0;
        foreach my $line ( @{ $output_lines{'code'} } ) {
            my $label = $line;
            chomp $label;
            if ( $is_label{$label} ) {
                next if !$is_target{$label};

                # place a semicolon between sequential labels
                # otherwise, a syntax error occurs
                if ($last_output_line_is_label) {
                    push @new_code, ";\n";
                }
                $last_output_line_is_label = 1;
            }
            elsif ( $line !~ /^\s*#/ && $line !~ /^\s*$/ ) {
                $last_output_line_is_label = 0;
            }
            push @new_code, $line;
        }
        $output_lines{'code'} = \@new_code;

        # push any remaining comments at the end of the code section
        if (@pending_comments) {
            push @{ $output_lines{'code'} }, @pending_comments;
        }
        @pending_comments = ();

        my $outer_block =
          (      $output_lines{'constant'}
              || $output_lines{'parameter_declaration'}
              || $output_lines{'parameter_data'}
              || $output_lines{'save'}
              || $output_lines{'common'}
              || $output_lines{'data'} );

        if ($outer_block) {
            push @output, "\n";
            push @output, " { $outer_block_name\n ";
        }

        if ( $output_lines{'constant'} ) {
            push @output, " \n ";
            push @output, @{ $output_lines{'constant'} };
        }

        if ( $output_lines{'parameter_declaration'} ) {
            push @output, @{ $output_lines{'parameter_declaration'} };
        }

        if ( $output_lines{'parameter_data'} ) {
            push @output, " BEGIN { \n ";
            push @output, @{ $output_lines{'parameter_data'} };
            push @output, " } \n ";
        }

        if ( $output_lines{'save'} ) {
            push @output, @{ $output_lines{'save'} };
        }

        if ( $output_lines{'common'} ) {
            push @output, " \n ";
            push @output, @{ $output_lines{'common'} };
        }

        if ( $output_lines{'data'} ) {
            push @output, " BEGIN { \n ";
            push @output, @{ $output_lines{'data'} };
            push @output, " } \n ";
        }

        if ($outer_block) { push @output, "\n"; }

        # combine the main sections in order
        foreach (qw(subroutine args use_vars declaration format code)) {

            if ( $output_lines{$_} ) {
                if ( $_ eq 'args' ) {
                    push @output, "\n";
                    push @output, "# call arg declarations\n";
                }
                push @output, @{ $output_lines{$_} };
            }
        }

        # close any outer block
        if ($outer_block) {
            push @output, "}\n";
        }

        return \@output;
    }
}    # end of output routines
1;
__END__
