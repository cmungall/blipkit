#!/usr/bin/perl

my $id;
while (<>) {
    if (/^id:\s+(\S+)/) {
        $id=$1;
        $term{$1} = $_;
    }
    if (/^namespace:\s+(\S+)/) {
        $ns{$id} = $1;
    }
    push(@all,$_);
}

foreach (@all) {
    if (/^id:\s+(\S+)/) {
        $id=$1;  
    }
    if (/^relationship: \w+ (\S+)/) {
        print STDERR "$1 $id // $ns{$1} $ns{$id}\n";
        if ($ns{$1} ne $ns{$id}) {
        }
        else {
            print "$_";
        }
    }
    else {
            print "$_";
        
    }
}
