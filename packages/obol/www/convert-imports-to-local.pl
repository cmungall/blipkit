#!/usr/bin/perl -w

if (!@ARGV) {
    usage();
    exit 0;
}

my $base = 'http://purl.org/obo/';
while ($ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '--base') {
        $base = shift;
    }
}
if ($base !~ /\/$/) {
    $base .= "/";
}
my $f = shift || die;

open(F,$f) || die $f;
while(<F>) {
    if (/(.*)$base(.*)/o) {
        my $suffix = $2;
        if (-f $suffix) {
            $_ = "$1file:$suffix\n";
        }
    }
    print;
}
close(F);
exit(0);
