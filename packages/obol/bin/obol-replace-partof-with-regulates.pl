#!/usr/bin/perl

use strict;
$/='[Term]';

while (<>) {
    my @lines = split(/\n/,$_);
    foreach my $line (@lines) {
        if ($line =~ /relationship:\s+part_of\s+(\S+)/) {
            my $to = $1;
            # is this a real part_of, or a part_of that is truly a regulates?
            if (/intersection_of:\s+regulates\s+$to/) {
                # get rid of it - reasoner will add regulates relations
                #s/relationship:\s+part_of\s+$to.*\n?//;
                s/relationship:\s+part_of\s$to/relationship: regulates $to/;
            }
        }
    }
    print;
}
