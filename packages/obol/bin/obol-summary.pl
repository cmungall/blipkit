#!/usr/bin/perl

my %n = ();
my %children = ();
while (<>) {
    if (/\"(.*)\"\s+\[(\S+)\]\s+is_a\s+\"(.*)\"\s+\[(\S+)\]/) {
        $n{$2} = $1;
        $n{$4} = $3;
        push(@{$children{$4}}, $2);
    }
    else {
        print STDERR "??: $_";
    }
}
foreach my $k (keys %children) {
    print "** $k $n{$k} **\n";
    #print "  children (is_a):\n";
    foreach (@{$children{$k}}) {
        print "$_ $n{$_}\n";
    }
    print "\n";
}
