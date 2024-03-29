# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: GF2.pm 4 2010-09-26 00:06:41Z demetri $

package Math::ModInt::GF2;

use 5.006;
use strict;
use warnings;

# ----- object definition -----

# Math::ModInt::GF2=ARRAY(...)

# .......... index ..........   # .......... value ..........
use constant F_RESIDUE => 0;    # residue r, either 0 or 1
use constant NFIELDS   => 1;

# ----- class data -----

BEGIN {
    require Math::ModInt;
    our @ISA     = qw(Math::ModInt);
    our $VERSION = '0.001';
}

my @base = map { bless [$_] } 0..1;     # singletons

# ----- private methods -----

sub _NEG { $_[0] }

sub _ADD {
    my ($this, $that) = @_;
    return $base[$this->[F_RESIDUE] ^ $that->[F_RESIDUE]];
}

sub _SUB {
    my ($this, $that) = @_;
    return $base[$this->[F_RESIDUE] ^ $that->[F_RESIDUE]];
}

sub _MUL {
    my ($this, $that) = @_;
    return $base[$this->[F_RESIDUE] & $that->[F_RESIDUE]];
}

sub _DIV {
    my ($this, $that) = @_;
    return $that->[F_RESIDUE]? $this: Math::ModInt->undefined;
}

sub _POW {
    my ($this, $exp) = @_;
    return
        $this->[F_RESIDUE] || $exp > 0? $this:
        $exp? $this->undefined: $base[1];
}

sub _INV {
    my ($this) = @_;
    return $this->[F_RESIDUE]? $this: Math::ModInt->undefined;
}

sub _NEW {
    my ($this, $residue) = @_;
    return $base[$residue & 1];
}

sub residue {
    my ($this) = @_;
    return $this->[F_RESIDUE];
}

sub modulus {
    return 2;
}

1;

__END__

=head1 NAME

Math::ModInt::GF2 - integer arithmetic modulo 2

=head1 VERSION

This documentation refers to version 0.001 of Math::ModInt::GF2.

=head1 SYNOPSIS

  use Math::ModInt;

  $a = Math::ModInt->new(1, 2);                   # 1 [mod 2]
  $b = $a->new(0);                                # 0 [mod 2]
  $c = $a + $b;                                   # 1 [mod 2]
  $d = $a**2 - $b/$a;                             # 1 [mod 2]

  print $d->residue, " [mod ", $b->modulus, "]";  # prints 1 [mod 2]
  print "$d";                                     # prints mod(1, 2)

  $bool = $c == $d;                               # true

=head1 DESCRIPTION

Math::ModInt::GF2 is an implementation of Math::ModInt for modulus
two.  Like all Math::ModInt implementations, it is loaded behind
the scenes when there is demand for it, without applications needing
to worry about it.  Implementations for special cases like this can
take advantage of properties specific to their subdomain and be
therefore substantially more efficient than generic ones.

=head1 SEE ALSO

=over 4

=item *

L<Math::ModInt>

=back

=head1 AUTHOR

Martin Becker, E<lt>becker-cpan-mp@cozap.comE<gt>

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2009-2010 by Martin Becker.  All rights reserved.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.6.0 or,
at your option, any later version of Perl 5 you may have available.

=head1 DISCLAIMER OF WARRANTY

This library is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.

=cut
