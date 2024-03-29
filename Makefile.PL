# Copyright (c) 2009-2012 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: Makefile.PL 36 2012-08-26 22:25:41Z demetri $

use strict;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME               => 'Math::ModInt',
    VERSION_FROM       => 'lib/Math/ModInt.pm',
    PREREQ_PM          => {
        'overload'     => 0,
        'Carp'         => 0,
        'Math::BigInt' => '1.991',      # '>= 1.58, != 1.99',
    },
    ABSTRACT_FROM      => 'lib/Math/ModInt.pm',
    AUTHOR             => 'Martin Becker <becker-cpan-mp@cozap.com>',
    $ExtUtils::MakeMaker::VERSION >= 6.18? (
        SIGN           => 1,
    ): (),
    $ExtUtils::MakeMaker::VERSION >= 6.31? (
        LICENSE        => 'perl',
    ): (),
    $ExtUtils::MakeMaker::VERSION >= 6.46? (
        META_MERGE => {
            recommends => {
                'Math::BigInt::GMP' => 0,
            }
        },
    ): (),
    $ExtUtils::MakeMaker::VERSION >= 6.47? (
        MIN_PERL_VERSION => '5.006',
    ): (),
    $ExtUtils::MakeMaker::VERSION >= 6.56? (
        BUILD_REQUIRES => {
            'ExtUtils::MakeMaker' => 0,
            'File::Basename'      => 0,
            'File::Spec'          => 0,
            'FindBin'             => 0,
            'Scalar::Util'        => 0,
            'Test'                => 0,
        },
    ): (),
);

__END__
