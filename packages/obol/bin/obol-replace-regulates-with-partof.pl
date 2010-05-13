#!/usr/bin/perl

while (<>) {
    s/^relationship:\s+\w+regulates\s+.*//;
    s/^relationship:\s+regulates\s+/relationship: part_of /;
    print;
}
