#!/usr/bin/perl -w
use strict;

# Michael E. Sparks, 11-14-16

# Just some grungy Perl code to scrape together
# a tab-delimited results file

open(DE,"<Dumb_End.txt") or die "$!\n";
open(DM,"<Dumb_Max.txt") or die "$!\n";
open(DH,"<Dumb_Hands.txt") or die "$!\n";

open(IE,"<Intel_End.txt") or die "$!\n";
open(IM,"<Intel_Max.txt") or die "$!\n";
open(IH,"<Intel_Hands.txt") or die "$!\n";

print "EndCashDumb\tMaxCashDumb\tEnd2MaxDumb\tHandsDealtDumb\t";
print "EndCashIntel\tMaxCashIntel\tEnd2MaxIntel\tHandsDealtIntel\n";

my($de,$dh,$dm,$ie,$ih,$im);
while($de=<DE>) {
  chomp($de);
  $de=~/\$(\d+)$/;
  my $end=$1;
  print "$end\t";

  $dm=<DM>;
  chomp($dm);
  $dm=~/\$(\d+)$/;
  my $max=$1;
  printf("%d\t%.5f\t",$max,$end/$max);

  $dh=<DH>;
  chomp($dh);
  $dh=~/: (\d+)$/;
  print "$1\t";

  $ie=<IE>;
  chomp($ie);
  $ie=~/\$(\d+)$/;
  $end=$1;
  print "$end\t";

  $im=<IM>;
  chomp($im);
  $im=~/\$(\d+)$/;
  $max=$1;
  printf("%d\t%.5f\t",$max,$end/$max);

  $ih=<IH>;
  chomp($ih);
  $ih=~/: (\d+)$/;
  print "$1\n";
}

close DE;
close DM;
close DH;
close IE;
close IM;
close IH;

exit 0;
