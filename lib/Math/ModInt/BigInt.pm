# Copyright (c) 2009-2010 Martin Becker.  All rights reserved.
# This package is free software; you can redistribute it and/or modify it
# under the same terms as Perl itself.
#
# $Id: BigInt.pm 4 2010-09-26 00:06:41Z demetri $

package Math::ModInt::BigInt;

use 5.006;
use strict;
use warnings;
use Carp qw(croak);
use Math::BigInt;

# ----- object definition -----

# Math::ModInt::BigInt=ARRAY(...)

# .......... index ..........   # .......... value ..........
use constant F_RESIDUE => 0;    # residue r, 0 <= r < m
use constant F_MODULUS => 1;    # modulus m
use constant NFIELDS   => 2;

# ----- class data -----

BEGIN {
    require Math::ModInt;
    our @ISA     = 'Math::ModInt';
    our $VERSION = '0.001';
}

# ----- overridden methods -----

sub _NEG {
    my ($this) = @_;
    return $this->_NEW(-$this->residue);
}

sub _ADD {
    my ($this, $that) = @_;
    return $this->_NEW($this->residue + $that->residue);
}

sub _SUB {
    my ($this, $that) = @_;
    return $this->_NEW($this->residue - $that->residue);
}

sub _MUL {
    my ($this, $that) = @_;
    return $this->_NEW($this->residue * $that->residue);
}

sub _DIV {
    my ($this, $that) = @_;
    my $i = $that->residue->copy->bmodinv($that->modulus);
    return $this->undefined if $i->is_nan;
    return $this->_NEW($this->residue * $i);
}

sub _POW {
    my ($this, $exp) = @_;
    if ($this->is_zero) {
        # work around Math::BigInt bug rt.cpan.org #61543
        return $this->undefined if 0 >  $exp;
        return $this->_NEW(1)   if 0 == $exp;
        return $this;
    }
    my $p = $this->residue->copy->bmodpow($exp, $this->modulus);
    return $this->undefined if $p->is_nan;
    return $this->_NEW($p);
}

sub _INV {
    my ($this) = @_;
    my $i = $this->residue->copy->bmodinv($this->modulus);
    return $this->undefined if $i->is_nan;
    return $this->_NEW($i);
}

sub _NEW {
    my ($this, $residue, $modulus) = @_;
    my $class = ref $this;
    if ($class) {
        $modulus = $this->modulus;
    }
    else {
        $class = $this;
    }
    foreach my $arg ($residue, $modulus) {
        if (!ref($arg) || !$arg->isa('Math::BigInt')) {
            $arg = Math::BigInt->new($arg);
        }
    }
    return bless [$residue % $modulus, $modulus], $class;
}

sub residue {
    my ($this) = @_;
    return $this->[F_RESIDUE];
}

sub modulus {
    my ($this) = @_;
    return $this->[F_MODULUS];
}

1;

__END__

=head1 NAME

Math::ModInt::BigInt - modular integer arithmetic, powered by Math::BigInt

=head1 VERSION

This documentation refers to version 0.001 of Math::ModInt::BigInt.

=head1 SYNOPSIS

  use Math::ModInt qw(mod);

  $a = Math::ModInt->new(3, 76543);               # 3 [mod 76543]
  $a = mod(3, 76543);                             # 3 [mod 76543]
  $b = $a->new(4);                                # 4 [mod 76543]
  $c = $a + $b;                                   # 7 [mod 76543]
  $d = $a**2 - $b/$a;                             # 25522 [mod 76543]

  print $a->residue, " [mod ", $b->modulus, "]";  # prints 3 [mod 76543]
  print "$a";                                     # prints mod(3, 76543)

  $bool = $c == $d;                               # false

=head1 DESCRIPTION

Math::ModInt::BigInt is a generic implementation of Math::ModInt for
arbitrarily large moduli.  Like all Math::ModInt implementations,
it is loaded behind the scenes when there is demand for it, without
applications needing to worry about it.

Note, however, that values returned by I<residue> or I<modulus> may
be Math::BigInt objects rather than plain Perl numbers when this
implementation has actually been involved.

=head1 SEE ALSO

L<Math::ModInt>, L<Math::BigInt>.

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
