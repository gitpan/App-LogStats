package App::LogStats;
use strict;
use warnings;
use File::Spec;
use Getopt::Long qw/GetOptionsFromArray/;
use IO::Interactive qw/is_interactive/;

our $VERSION = '0.06';

use Class::Accessor::Lite (
    new => 1,
    rw  => [qw/
        result
        config
    /],
);

our @RESULT_LIST = (qw/
    count sum _line_ average median mode _line_ max min range
/);
our %MORE_RESULT = (
    median => 1,
    mode   => 1,
);

our @DRAW_TABLE = (
    [' ',' ','-',' '],
    [' ',' ',' '],
    [' ',' ','-',' '],
    [' ',' ',' '],
    [' ',' ','-',' '],
    [' ',' ','-',' '],
);

sub run {
    my $self = shift;
    $self->_prepare(\@_)->_main->_finalize;
}

sub _prepare {
    my ($self, $argv) = @_;

    my $config = $self->_set_rc( $self->_rc_file($argv) );
    $self->_merge_opt($config, $argv);

    $self->config($config);

    $self;
}

sub _rc_file {
    my ($self, $argv) = @_;

    my $rc = 0;
    for my $opt (@{$argv}) {
        return $opt if $rc == 1;
        $rc = 1 if $opt eq '--rc';
    }
    return '.statsrc';
}

sub _set_rc {
    my ($self, $rc_file) = @_;

    my %config;

    for my $dir ($ENV{STATSRC_DIR}, $ENV{HOME}, '.') {
        next unless $dir;
        my $file = File::Spec->catfile($dir, $rc_file);
        next unless -e $file;
        $self->_read_rc($file => \%config);
    }

    return \%config;
}

sub _read_rc {
    my ($self, $file, $config) = @_;

    open my $fh, '<', $file or die "Could not open file: $file";
    while (<$fh>) {
        chomp;
        next if /\A\s*\Z/sm;
        if (/\A(\w+):\s*(.+)\Z/sm) {
            my ($key, $value) = ($1, $2);
            if ($key eq 'file') {
                push @{$config->{$key}}, $value;
            }
            else {
                $config->{$key} = $value;
            }
        }
    }
    close $fh;
}

sub _merge_opt {
    my ($self, $config, $argv) = @_;

    Getopt::Long::Configure('bundling');
    GetOptionsFromArray(
        $argv,
        'file=s@'       => \$config->{file},
        'd|delimiter=s' => \$config->{delimiter},
        'f|fields=s'    => \$config->{fields},
        't|through'     => \$config->{through},
        'di|digit=i'    => \$config->{digit},
        's|strict'      => \$config->{strict},
        'no-comma'      => \$config->{no_comma},
        'tsv'           => \$config->{tsv},
        'csv'           => \$config->{csv},
        'more'          => \$config->{more},
        'cr'            => \$config->{cr},
        'crlf'          => \$config->{crlf},
        'rc=s'          => \$config->{rc},
        'h|help'        => sub {
            $self->_show_usage(1);
        },
        'v|version' => sub {
            print "stats v$App::LogStats::VERSION\n";
            exit 1;
        },
    ) or $self->_show_usage(2);

    push @{$config->{file}}, @{$argv};

    $self->_validate_config($config);
}

sub _show_usage {
    my ($self, $exitval) = @_;

    require Pod::Usage;
    Pod::Usage::pod2usage($exitval);
}

sub _validate_config {
    my ($self, $config) = @_;

    if (!$config->{digit} || $config->{digit} !~ m!^\d+$!) {
        $config->{digit} = 2;
    }

    $config->{delimiter} = "\t" unless defined $config->{delimiter};

    if ($config->{fields}) {
        for my $f ( split ',', $config->{fields} ) {
            $config->{field}->{$f} = 1;
        }
        delete $config->{fields};
    }
    else {
        $config->{field}->{1} = 1;
    }
}

sub _main {
    my $self = shift;

    my $r = +{};

    if ( !is_interactive() ) {

        while ( my $line = <STDIN> ) {
            $self->_loop(\$line => $r);
        }

    }
    elsif ( scalar @{ $self->config->{file} } ) {

        for my $file (@{$self->config->{file}}) {
            open my $fh, '<', $file or die "$file: No such file";
            while ( my $line = <$fh> ) {
                $self->_loop(\$line => $r);
            }
            close $fh;
        }

    }

    $self->_after_calc($r);

    $self->result($r);
    $self;
}

sub _loop {
    my ($self, $line_ref, $r) = @_;

    my $line = $$line_ref;

    print $line if $self->config->{through};
    chomp $line;
    return unless $line;
    $self->_calc_line($r, [ split $self->config->{delimiter}, $line ]);
}

sub _calc_line {
    my ($self, $r, $elements) = @_;

    my $strict = $self->config->{strict};
    my $i = 0;
    for my $element (@{$elements}) {
        $i++;
        next unless $self->config->{field}{$i};
        if ( (!$strict && $element =~ m!\d!)
                || ($strict && $element =~ m!^(\d+\.?(:?\d+)?)$!) ) {
            my ($num) = ($element =~ m!^(\d+\.?(:?\d+)?)!);
            $num ||= 0; # FIXME
            $r->{$i}{count}++;
            $r->{$i}{sum} += $num;
            $r->{$i}{max}  = $num
                if !defined $r->{$i}{max} || $num > $r->{$i}{max};
            $r->{$i}{min} = $num
                if !defined $r->{$i}{min} || $num < $r->{$i}{min};
            push @{$r->{$i}{list}}, $num if $self->config->{more};
        }
    }
}

sub _after_calc {
    my ($self, $r) = @_;

    for my $i (keys %{$r}) {
        next unless $r->{$i}{count};
        $r->{$i}{average} = $r->{$i}{sum} / $r->{$i}{count};
        if ($self->config->{more}) {
            $r->{$i}{median} = $self->_calc_median($r->{$i}{list});
            $r->{$i}{mode}   = $self->_calc_mode($r->{$i}{list});
        }
        $r->{$i}{range}   = $r->{$i}{max} - $r->{$i}{min};
        $r->{show_result} ||= 1;
    }
}

sub _calc_median {
    my ($self, $list) = @_;

    return unless ref $list eq 'ARRAY';
    return $list->[0] unless @{$list} > 1;
    @{$list} = sort { $a <=> $b } @{$list};
    return $list->[ $#{$list} / 2 ] if @{$list} & 1;
    my $mid = @{$list} / 2;
    return ( $list->[ $mid - 1 ] + $list->[ $mid ] ) / 2;
}

sub _calc_mode {
    my ($self, $list) = @_;

    return unless ref $list eq 'ARRAY';
    return $list->[0] unless @{$list} > 1;
    my %hash;
    $hash{$_}++ for @{$list};
    my $max_val = ( sort { $b <=> $a } values %hash )[0];
    for my $key (keys %hash) {
        delete $hash{$key} unless $hash{$key} == $max_val;
    }
    return $self->_calc_average([keys %hash]);
}

sub _calc_average {
    my ($self, $list) = @_;

    my $sum = 0;
    for my $i (@{$list}) {
        $sum += $i;
    }
    return $sum / scalar(@{$list});
}

sub _finalize {
    my $self = shift;

    return unless $self->result->{show_result};

    my $output_lines;
    if ($self->config->{tsv}) {
        $output_lines = $self->_view_delimited_line("\t");
    }
    elsif ($self->config->{csv}) {
        $output_lines = $self->_view_delimited_line(',');
    }
    else {
        $output_lines = $self->_view_table;
    }

    my $lf = $self->config->{cr} ? "\r" : $self->config->{crlf} ? "\r\n" : "\n";

    print $lf unless $self->config->{quiet};
    for my $line ( @{$output_lines} ) {
        print $line, $lf;
    }
}

sub _view_delimited_line {
    my ($self, $delimiter) = @_;

    my @fields = sort keys %{$self->config->{field}};
    my @output;
    push @output, join($delimiter, '', map { $self->_quote($_) } @fields);
    for my $col (@RESULT_LIST) {
        next if !$self->config->{more} && $MORE_RESULT{$col};
        next if $col eq '_line_';
        my @rows = ( $self->_quote($col) );
        for my $i (@fields) {
            push @rows, $self->_quote( $self->_normalize($self->result->{$i}{$col}) );
        }
        push @output, join($delimiter, @rows);
    }
    return \@output;
}

sub _view_table {
    my $self = shift;

    my @fields = sort keys %{$self->config->{field}};

    require Text::ASCIITable;
    my $t = Text::ASCIITable->new;
    $t->setCols('', @fields);
    for my $col (@RESULT_LIST) {
        next if !$self->config->{more} && $MORE_RESULT{$col};
        if ($col eq '_line_') {
            $t->addRowLine;
            next;
        }
        my @rows;
        for my $i (@fields) {
            push @rows, $self->_normalize($self->result->{$i}{$col});
        }
        $t->addRow($col, @rows);
    }
    return [ split( "\n", $t->draw(@DRAW_TABLE) ) ];
}

sub _quote {
    my ($self, $value) = @_;

    return $value unless $self->config->{csv};
    return '"'. $value. '"';
}

sub _normalize {
    my ($self, $value) = @_;

    return '-' unless defined $value;

    if ($value =~ m!\.!) {
        $value = sprintf("%.". $self->config->{digit}. 'f',  $value);
    }

    unless ($self->config->{no_comma}) {
        my ($n, $d) = split /\./, $value;
        while ( $n =~ s!(.*\d)(\d\d\d)!$1,$2! ){};
        $value = $d ? "$n\.$d" : $n;
    }

    return $value;
}

1;

__END__

=head1 NAME

App::LogStats - calculate lines


=head1 SYNOPSIS

    use App::LogStats;

    my $stats = App::LogStats->new->run(@ARGV);


=head1 DESCRIPTION

App::LogStats helps you to calculate data from lines.

See: L<stats> command


=head1 METHODS

=head2 run

to run command


=head1 REPOSITORY

App::LogStats is hosted on github
<http://github.com/bayashi/App-LogStats>


=head1 AUTHOR

Dai Okabayashi E<lt>bayashi@cpan.orgE<gt>


=head1 SEE ALSO

L<stats>

few stats codes were copied from L<Statistics::Lite>


=head1 LICENSE

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself. See L<perlartistic>.

=cut
