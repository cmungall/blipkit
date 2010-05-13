#!/usr/bin/perl

while (<>) {
    print;
    if (/intersection_of:\s+(.*regulates)\s+(.+)/) {
        print "relationship: $1 $2\n";
    }
}
