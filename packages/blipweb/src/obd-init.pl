#!/usr/bin/perl -w
use strict;

my @confs = qw(conf_obd conf_ct conf_obol);

my $killmode = 0;
while (@ARGV && $ARGV[0] =~ /^\-/) {
    my $opt = shift @ARGV;
    if ($opt eq '-k' || $opt eq '--kill') {
        $killmode = 1;
    }
    else {
        die $opt;
    }
}

foreach (@confs) {
    start_server($_);
}
exit 0;

sub start_server {
    my $conf = shift;
    my @ps = split(/\n/,`ps auxwww`);
    my @matches = grep {/\-c $conf/} @ps;
    if (@matches) {
        print STDERR "Already running: $conf [@matches]\n";
        if ($killmode) {
            if (@matches>1) {
                die ">1 match: @matches";
            }
            my $m = shift @matches;
            if ($m =~ /^\w+\s+(\d+).*\-c $conf/) {
                my $pid = $1;
                runcmd("kill -9 $pid && sleep 2") && die "cannot kill $pid";
            }
            else {
                die "cant get process ID";
            }
            startup($conf);
        }

    }
    else {
        startup($conf);
    }
}

sub startup {
    my $conf = shift;
    runcmd("cp log.$conf log.$conf.old");
    runcmd("run-amigod -bg -c $conf -u http://yuri.lbl.gov >& log.$conf &") && die "could not start process";
}

sub runcmd {
    my $cmd = shift;
    print STDERR "CMD: $cmd\n";
    my $err = system($cmd);
    return $err;
}
