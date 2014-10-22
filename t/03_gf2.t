# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: 03_gf2.t 4 2010-09-26 00:06:41Z demetri $

# Tests of the Math::ModInt::GF2 subclass of Math::ModInt.

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/03_gf2.t'

#########################

use strict;
use warnings;
use Test;
BEGIN { plan tests => 11 };
use Math::ModInt qw(mod);

#########################

my @space = map { mod($_, 2) } 0..1;

sub check_unary {
    my ($op, $code, $results) = @_;
    my @res = @space[split //, $results];
    my $ok = 1;
    foreach my $a (@space) {
        my $want = shift @res;
        my $got = $code->($a);
        $ok &&=
            defined($want)?
                $got->is_defined && $got == $want:
                $got->is_undefined;
        if (!$ok) {
            $want = Math::ModInt->undefined if !defined $want;
            print "# $op $a: got $got, expected $want\n";
            last;
        }
    }
    ok($ok);
}

sub check_binary {
    my ($op, $code, $results) = @_;
    my @res = @space[split //, $results];
    my $ok = 1;
    foreach my $a (@space) {
        foreach my $b (@space) {
            my $want = shift @res;
            my $got = $code->($a, $b);
            $ok &&=
                defined($want)?
                    $got->is_defined && $got == $want:
                    $got->is_undefined;
            if (!$ok) {
                $want = Math::ModInt->undefined if !defined $want;
                print "# $a $op $b: got $got, expected $want\n";
                last;
            }
        }
    }
    ok($ok);
}

sub check_lefty {
    my ($op, $code, $args, $results) = @_;
    my @res = @space[split //, $results];
    my $ok = 1;
    foreach my $a (@space) {
        foreach my $b (@{$args}) {
            my $want = shift @res;
            my $got = $code->($a, $b);
            $ok &&=
                defined($want)?
                    $got->is_defined && $got == $want:
                    $got->is_undefined;
            if (!$ok) {
                $want = Math::ModInt->undefined if !defined $want;
                print "# $a $op $b: got $got, expected $want\n";
                last;
            }
        }
    }
    ok($ok);
}

sub check_attr {
    my ($method, $results) = @_;
    my @res = @{$results};
    my $ok = 1;
    foreach my $a (@space) {
        my $want = shift @res;
        my $got = $a->$method;
        $ok &&=
            defined($want)?
                defined($got) && $got == $want:
                !defined($got);
        if (!$ok) {
            print "# $method $a: got $got, expected $want\n";
            last;
        }
    }
    ok($ok);
}

check_lefty('new', sub { $_[0]->new($_[1]) }, [-1, 0, 1, 2], '10101010');
check_unary('neg', sub { -$_[0] }, '01');
check_unary('inv', sub { $_[0]->inverse }, '21');
check_binary('+', sub { $_[0] + $_[1] }, '0110');
check_binary('-', sub { $_[0] - $_[1] }, '0110');
check_binary('*', sub { $_[0] * $_[1] }, '0001');
check_binary('/', sub { $_[0] / $_[1] }, '2021');
check_lefty('**', sub { $_[0] ** $_[1] }, [-1, 0, 1, 2], '21001111');
check_attr('residue',        [0, 1]);
check_attr('signed_residue', [0, -1]);
check_attr('modulus',        [2, 2]);

__END__
