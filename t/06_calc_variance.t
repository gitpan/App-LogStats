use strict;
use warnings;
use Test::More;

use App::LogStats;

test_variance([0], 0);
test_variance([1], 0);
test_variance([0, 1], 0.5);
test_variance([0, 0, 1], qr/^0\.33333/);
test_variance([0, 1, 1], qr/^0\.33333/);
test_variance([0, 1, 2], 1);
test_variance([1, 2, 3], 1);
test_variance([1, 2, 3, 3], qr/^0\.9166/);

done_testing;

sub test_variance {
    my ($list, $expect) = @_;

    my $stats = App::LogStats->new;
    my $r = +{
        0 => +{
            average => $stats->_calc_average($list),
            list    => $list,
        }
    };
    my $result = $stats->_calc_variance(0, $r);

    if (ref($expect) eq 'Regexp') {
        like $result, $expect, "list: @{$r->{0}{list}}";
    }
    else {
        is $result, $expect, "list: @{$r->{0}{list}}";
    }
}
