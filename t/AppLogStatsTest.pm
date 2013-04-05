package t::AppLogStatsTest;
use strict;
use warnings;
use Test::More;
use Test::Output;

use App::LogStats;

use parent qw/Exporter/;
our @EXPORT_OK = qw/ test_stats /;

sub test_stats {
    my ($expect, @cmd) = @_;

    my $stats = App::LogStats->new;
    stdout_is { $stats->run(@cmd); } $expect, join(' ', @cmd);
}

1;
