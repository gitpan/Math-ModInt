# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: 01_basics.t 4 2010-09-26 00:06:41Z demetri $

# Basic tests.

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/01_basics.t'

#########################

use strict;
use warnings;
use Test;
use Scalar::Util qw(blessed looks_like_number);
use Math::BigInt;
BEGIN { plan tests => 34 };
use Math::ModInt qw(mod);
ok(1);          # module loaded

#########################

sub check_mod {
    my ($obj, $r, $mod) = @_;
    my $ref = ref $obj;
    my $got = 
        !defined $obj?              'undef':
        !$ref &&
        looks_like_number($obj)?    "number $obj":
        !$ref?                      qq{scalar "$obj"}:
        !blessed($obj)?             "unblessed $ref ref":
        !$obj->isa('Math::ModInt')? "alien $ref object":
        !$obj->is_defined?          'Math::ModInt->undefined':
        $mod != $obj->modulus ||
        $r   != $obj->residue?      "$obj":
        '';
    if ('' ne $got) {
        print "# expected mod($r, $mod), got $got\n";
        return 0;
    }
    return 1;
}

my $a = mod(32, 127);
ok(check_mod($a, 32, 127));

my $b = $a->new(99);
ok(check_mod($b, 99, 127));

my $bb = $b;
$bb += 1;
ok(check_mod($bb, 100, 127));
ok($bb != $b);

my $c = $a + $b;
ok(check_mod($c, 4, 127));

my $d = $a**2 - $b/$a;
ok(check_mod($d, 120, 127));

my $e = $d + 0;
ok(check_mod($e, 120, 127));
my $bi = Math::BigInt->new('4');
my $bool;

ok($d == $e);
$bool = $d != $e;
ok(!$bool);
ok($c != $d);
ok($c == 4);
ok($c == 131);
ok(4 == $c);
ok($c != 5);
ok($c != 132);
ok(5 != $c);
ok($c == $bi);
ok($bi != $d);

my $f = mod(4, 128);
ok($c != $f);
$bool = $c == $f;
ok(!$bool);

++$f;
ok(check_mod($f, 5, 128));
ok($e != $f);
$bool = $e == $f;
ok(!$bool);

$f = $d->inverse;
ok(check_mod($f, 18, 127));

if ($f) {
    $bool = 1;
}
else {
    $bool = 0;
}
ok($bool);
$bool = !$f;
ok(!$bool);

$f = mod(0, 127);
if ($f) {
    $bool = 1;
}
else {
    $bool = 0;
}
ok(!$bool);
$bool = !$f;
ok($bool);

my $m = $d->modulus;
ok(127 == $m);

my $r = $d->residue;
ok(120 == $r);

my $s = $c->signed_residue;
ok(4 == $s);
$s = $d->signed_residue;
ok(-7 == $s);

my $t = "$a";
ok('mod(32, 127)' eq $t);

__END__
