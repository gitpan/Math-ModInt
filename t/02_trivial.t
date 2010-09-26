# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: 02_trivial.t 4 2010-09-26 00:06:41Z demetri $

# Tests of the Math::ModInt::Trivial subclass of Math::ModInt.

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/02_trivial.t'

#########################

use strict;
use warnings;
use Test;
BEGIN { plan tests => 33 };
use Math::ModInt qw(mod);

#########################

my $a = mod(0, 1);
ok(defined $a);
ok($a->isa('Math::ModInt'));
ok(0 == $a->residue);
ok(1 == $a->modulus);
ok(0 == $a->signed_residue);
ok('mod(0, 1)' eq "$a");
ok($a->is_defined);
ok(!$a->is_undefined);

my $b = $a->new(0);
ok($a == $b);
ok(not $a != $b);
ok(!$a);
ok($a? 0: 1);

my $c = -$a;
ok($c == $a);
my $d = $a->inverse;
ok($d == $a);
my $e = $a + $b;
ok($e == $a);
my $f = $a - $b;
ok($f == $a);
my $g = $a * $b;
ok($g == $a);
my $h = $a / $b;
ok($h == $a);
my $i = $a ** 3;
ok($i == $a);
my $j = $a ** 0;
ok($j == $a);
my $k = $a ** -1;
ok($k == $a);

my $el = $a + 1;
ok($el == $a);
my $er = 1 + $a;
ok($er == $a);
my $fl = $a - 1;
ok($fl == $a);
my $fr = 1 - $a;
ok($fr == $a);
my $gl = $a * 2;
ok($gl == $a);
my $gr = 2 * $a;
ok($gr == $a);
my $hl = $a / 2;
ok($hl == $a);
my $hr = 2 / $a;
ok($hr == $a);

my $il = eval { $a ** $b };
ok(!defined $il);
ok($@ =~ /integer exponent expected/);
my $ir = eval { 1 ** $a };
ok(!defined $ir);
ok($@ =~ /integer exponent expected/);

__END__
