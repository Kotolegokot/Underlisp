#!/usr/bin/env perl

my $directory = "tests";
my $bin = "dist/build/underlisp/underlisp";

`cabal build -j5` unless -e $bin;

opendir(DIR, $directory) or die $!;
while (my $file = readdir DIR) {
    if ($file =~ m/\.unlisp$/) {
        print "running '$directory/$file'...\n";
        `$bin $directory/$file`;
    }
}
closedir(DIR);
