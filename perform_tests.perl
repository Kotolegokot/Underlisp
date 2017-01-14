#!/usr/bin/env perl

my $directory = "tests";
my $bin = "dist/build/Underlisp/Underlisp";

`cabal build` unless -e $bin;

opendir(DIR, $directory) or die $!;
while (my $file = readdir DIR) {
    if ($file =~ m/\.unlisp$/) {
        print "running '$directory/$file'...\n";
        `dist/build/underlisp/underlisp $directory/$file`;
    }
}
closedir(DIR);
