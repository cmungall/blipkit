#!/usr/bin/perl
use strict;

my %base;
while (<>) {
    # 1 G   1(A) - G   2(A) -  s35  -    0
    if (/\s*(\d+)\s+(.)\s+(\d+)\(.\)\s+\-\s+(.)\s+(\d+)\(.\)\s+\-\s+(\S+)/) {
        my ($row,$b1,$p1,$b2,$p2,$rel) = ($1,$2,$3,$4,$5,$6);
        $base{$p1} = $b1;
        my $pred;
        if ($rel eq 'c35') {
            $pred = 'five_prime_to';
        }
        elsif ($rel =~ /^[a-zA-Z]+$/) {
            $pred = 'pairs_with_'.uc($rel);
            fact($pred,[$p1,$p2]);
        }
        else {
        }
        fact($pred,[$p1,$p2]);
    }
}
foreach my $p (sort {$a <=> $b} keys %base) {
    fact(lc($base{$p}),[$p]);
}

exit 0;

sub prologquote {
    my $s = shift;
    my $force = shift;
    if (ref($s)) {
        if (ref($s) ne 'HASH') {
            sprintf("[%s]",
                    join(',',map{prologquote($_)} @$s));
        }
        else {
            my @keys = keys %$s;
            if (@keys == 1) {
                my $functor = $keys[0];
                my $args = $s->{$functor};
                sprintf("$functor(%s)",
                        join(', ', map {prologquote($_)} @$args));
            }
            else {
                warn "@keys != 1 - ignoring";
            }
        }
    }
    else {
        $s = '' unless defined $s;
        if ($s =~ /^[\-]?[0-9]+$/ && !$force) {
            return $s;
        }
        $s =~ s/\'/\'\'/g;
        "'$s'";
    }
}


sub nl {
    print("\n");
}

sub fact {
    my $pred = shift;
    my @args = @{shift||[]};
    my $cmt = shift;
    printf("$pred(%s).",
           join(', ', map {prologquote($_)} @args));
    nl;
}


1;
