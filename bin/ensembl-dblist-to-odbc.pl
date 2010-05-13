#!/usr/bin/perl

my $host = 'ensembldb.ensembl.org';

#my @dbs = `echo "SHOW DATABASES" | mysql -h $host -u anonymous | sort`;
# latest dbs on port 5306
my @dbs = `echo "SHOW DATABASES" | mysql -h $host -u anonymous -P 5306 | sort`;
shift @dbs;
my %conf_by_db = ();
foreach (@dbs) {
    chomp;
    my @parts = split(/_/,$_);
    my $len = $#parts;
    
    my $n = join('_',@parts[(0..$len-2)]);
    my $v = join('_',@parts[($len-1..4)]);

    next unless $n;
    
    my $conf = "
[$n]
Driver       = /usr/local/lib/libmyodbc5.so
Description  = $_
Database     = $_
Server       = $host
User         = anonymous
Type         = MySQL
IncludeViews = Yes
port         = 5306
";

    $conf_by_db{$n} = $conf;  # overwrite prev version: we have sorted so it's OK
}
foreach (values %conf_by_db) {
  print $_;
}
