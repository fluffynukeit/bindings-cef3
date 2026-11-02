
# This perl script converts the many, many FunPtrs in CEF framework definition
# to individual callback types, so bindings-dsl will make mk' functions for 
# them.

use strict;
use warnings;

my $file = 'src/Bindings/CEF3.hsc';
open my $fh, $file or die "Could not open $file: $!";
open (my $oh, '>', "CEF3new.hsc") or die "Could not open new file!";

my $inType = '';
my @callbacks = ();

print "Starting processing...\n";
while ( my $line = <$fh>) {
  if (length($inType) == 0 && $line =~ /\#starttype\s+(\w*)_t\s*\n/) {
    $inType = $1;
    print "Processing: $inType\n";
    print $oh $line;
  } elsif ($line =~ /\#field\s+([^,]+),(\s+)FunPtr\s*\(([^\n]+)\)\n/) {
    my $cbType = "cb_${inType}_$1";
    push @callbacks, "#callback_t $cbType,$2$3";
    print $oh "#field $1,$2<$cbType>\n"
  } elsif ($line =~ /\#stoptype/) {
    print $oh $line;
    print $oh "\n";
    foreach (@callbacks) {
      print $oh "$_\n";
    }
    print $oh "\n";
    $inType = '';
    @callbacks = ();
  }
  else {
    print $oh $line; 
  }
}

