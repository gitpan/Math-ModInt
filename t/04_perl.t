# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: 04_perl.t 26 2010-10-03 12:32:28Z demetri $

# Tests of the Math::ModInt::Perl subclass of Math::ModInt.

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/04_perl.t'

#########################

use strict;
use warnings;
use Test;
BEGIN { plan tests => 41 };
use Math::ModInt qw(mod);

#########################

sub check_unary {
    my ($space, $op, $code, $results) = @_;
    my @res = @{$space}[split //, $results];
    my $ok = 1;
    foreach my $a (@{$space}) {
        my $want = shift @res;
        my $got = $code->($a);
        $ok &&=
            defined($want)?
                $got->is_defined && $got == $want:
                $got->is_undefined;
        if (!$ok) {
            $want = Math::ModInt->undefined if !defined $want;
            my $mod = $space->[0]->modulus;
            print "# $op $a (mod $mod): got $got, expected $want\n";
            last;
        }
    }
    ok($ok);
}

sub check_binary {
    my ($space, $op, $code, $results) = @_;
    my @res = @{$space}[split //, $results];
    my $ok = 1;
    BINARY:
    foreach my $a (@{$space}) {
        foreach my $b (@{$space}) {
            my $want = shift @res;
            my $got = $code->($a, $b);
            $ok &&=
                defined($want)?
                    $got->is_defined && $got == $want:
                    $got->is_undefined;
            if (!$ok) {
                $want = Math::ModInt->undefined if !defined $want;
                my $mod = $space->[0]->modulus;
                print "# $a $op $b (mod $mod): got $got, expected $want\n";
                last BINARY;
            }
        }
    }
    ok($ok);
}

sub check_lefty {
    my ($space, $op, $code, $args, $results) = @_;
    my @res = @{$space}[split //, $results];
    my $ok = 1;
    LEFTY:
    foreach my $a (@{$space}) {
        foreach my $b (@{$args}) {
            my $want = shift @res;
            my $got = $code->($a, $b);
            $ok &&=
                defined($want)?
                    $got->is_defined && $got == $want:
                    $got->is_undefined;
            if (!$ok) {
                $want = Math::ModInt->undefined if !defined $want;
                my $mod = $space->[0]->modulus;
                print "# $a $op $b (mod $mod): got $got, expected $want\n";
                last LEFTY;
            }
        }
    }
    ok($ok);
}

sub check_attr {
    my ($space, $method, $results) = @_;
    my @res = @{$results};
    my $ok = 1;
    foreach my $a (@{$space}) {
        my $want = shift @res;
        my $got = $a->$method;
        $ok &&=
            defined($want)?
                defined($got) && $got == $want:
                !defined($got);
        if (!$ok) {
            my $mod = $space->[0]->modulus;
            print "# $method $a (mod $mod): got $got, expected $want\n";
            last;
        }
    }
    ok($ok);
}

my @gf3 = map { mod($_, 3) } 0..2;
my @z4  = map { mod($_, 4) } 0..3;

my $m = mod(0, 3);
my $mm;
$mm = $m->optimize_default;
ok($mm == $m);
$mm = $m->optimize_time;
ok($mm == $m);

check_lefty(
    \@gf3, 'new', sub { $_[0]->new($_[1]) },
    [-2, -1, 0, 1, 2, 3],
    '120' x 6,
);
check_unary(\@gf3, 'neg', sub { -$_[0] }, '021');
check_unary(\@gf3, 'inv', sub { $_[0]->inverse }, '312');
check_binary(\@gf3, '+', sub { $_[0] + $_[1] }, '012120201');
check_binary(\@gf3, '-', sub { $_[0] - $_[1] }, '021102210');
check_binary(\@gf3, '*', sub { $_[0] * $_[1] }, '000012021');
check_binary(\@gf3, '/', sub { $_[0] / $_[1] }, '300312321');
check_lefty(
    \@gf3, '**', sub { $_[0] ** $_[1] },
    [-2, -1, 0, 1, 2, 3],
    '331000111111121212',
);
check_attr(\@gf3, 'residue',        [0, 1, 2]);
check_attr(\@gf3, 'signed_residue', [0, 1, -1]);
check_attr(\@gf3, 'modulus',        [3, 3, 3]);

$mm = $m->optimize_default;
ok($mm == $m);
$mm = $m->optimize_space;
ok($mm == $m);

check_unary(\@gf3, 'inv', sub { $_[0]->inverse }, '312');

$mm = $m->optimize_default;
ok($mm == $m);

check_lefty(
    \@z4, 'new', sub { $_[0]->new($_[1]) },
    [-3, -2, -1, 0, 1, 2, 3, 4],
    '1230' x 8,
);
check_unary(\@z4, 'neg', sub { -$_[0] }, '0321');
check_unary(\@z4, 'inv', sub { $_[0]->inverse }, '4143');
check_binary(\@z4, '+', sub { $_[0] + $_[1] }, '0123123023013012');
check_binary(\@z4, '-', sub { $_[0] - $_[1] }, '0321103221033210');
check_binary(\@z4, '*', sub { $_[0] * $_[1] }, '0000012302020321');
check_binary(\@z4, '/', sub { $_[0] / $_[1] }, '4040414342424341');
check_lefty(
    \@z4, '**', sub { $_[0] ** $_[1] },
    [-2, -1, 0, 1, 2, 3],
    '441000111111441200131313',
);
check_attr(\@z4, 'residue',        [0, 1, 2, 3]);
check_attr(\@z4, 'signed_residue', [0, 1, -2, -1]);
check_attr(\@z4, 'modulus',        [4, 4, 4, 4]);

$m = mod(3, 257);

$mm = $m->optimize_default;
ok($mm == $m);
$mm = $m->optimize_time;
ok($mm == $m);
$mm = $m->optimize_time;
ok($mm == $m);

$mm = $m ** -1;
ok(86 == $mm);
$mm = $m ** -2;
ok(200 == $mm);

$mm = $m->optimize_space;
ok($mm == $m);

$mm = $m ** -1;
ok(86 == $mm);
$mm = $m ** -2;
ok(200 == $mm);

$mm = $m->optimize_default;
ok($mm == $m);

$mm = $m ** -1;
ok(86 == $mm);
$mm = $m ** -2;
ok(200 == $mm);

$m = mod(3, 46337);
my $i = $m ** 181;
my $ip = 1;
foreach my $j (1..8) {
    $ip &&= $i->residue > 1;
    $i *= $i;
}
ok($ip && 1 == $i);

$m = mod(1, 32771);
$mm = $m->optimize_time;
ok($mm == $m);

__END__
