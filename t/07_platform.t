# Copyright (c) 2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: 07_platform.t 30 2010-10-03 14:02:34Z demetri $

# Gather platform information to help analyzing test reports.

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/07_platform.t'

#########################

use strict;
use warnings;
use Test;
use Config;

plan(tests => 1);

#########################

print "# perl version is ", $], "\n";
print "# OS name is ", $^O, "\n";

foreach my $module (qw(
    Math::ModInt
    Math::ModInt::BigInt
    Math::ModInt::ChineseRemainder
    Math::ModInt::Event
    Math::ModInt::Event::Trap
    Math::ModInt::GF2
    Math::ModInt::Perl
    Math::ModInt::Trivial
    overload
    Carp
    Math::BigInt
)) {
    if (eval "require $module") {
        my $version = eval { $module->VERSION };
        if (defined $version) {
            print "# module $module has version $version\n";
        }
        else {
            print "# module $module has no version number\n";
        }
    }
    else {
        print "# module $module not available\n";
    }
}

my ($ivsize, $nvsize) = @Config{'ivsize', 'nvsize'};
print "# ivsize is $ivsize, nvsize is $nvsize\n";

my $max_modulus = Math::ModInt::_MAX_MODULUS_PERL();
print "# _MAX_MODULUS_PERL is $max_modulus\n";

ok(1);

__END__
