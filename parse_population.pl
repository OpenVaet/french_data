#!/usr/bin/perl
use strict;
use warnings;
use 5.30.0;
no autovivification;
binmode STDOUT, ":utf8";
use utf8;
use open ':std', ':encoding(UTF-8)';
use Data::Printer;
use Data::Dumper;
use JSON;
use Encode;
use Encode::Unicode;
use Scalar::Util qw(looks_like_number);
use Math::Round qw(nearest);
use Text::CSV qw( csv );
use FindBin;
use lib "$FindBin::Bin/../../lib";

my @sexes      = ('All', 'Men', 'Women');
my @age_groups = ('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-94', '95+', 'Total');

my %departments = ();
load_departments_data();

open my $out, '>:utf8', 'data/population_by_age_and_department.csv';
say $out "year,department,sex,age_group,value";
my %population = ();
for my $file (glob "population/*") {
	parse_file($file);
}
close $out;

sub load_departments_data {
	my $file = 'data/departments.json';
	my $json;
	open my $in, '<:', $file;
	while (<$in>) {
		$json .= $_;
	}
	close $in;
	$json = decode_json($json);
	%departments = %$json;
}

sub parse_file {
	my $file   = shift;
	my ($year) = $file =~ /population\/(.*)\.csv/;
	open my $in, '<:utf8', $file;
	while (<$in>) {
		chomp $_;
		my @values = split ',', $_;
		my $values = scalar @values;
		if ($values == 65) {
			my $department = $values[0] // die;
			my $e_num      = 1;
			for my $sex (@sexes) {
				for my $age_group (@age_groups) {
					$e_num++;
					my $value = $values[$e_num] // die;
					next unless exists $departments{$department};
					if ($sex eq 'All') {
						say "$year - $department - $sex - $age_group - $value";
						say $out "$year,$department,$sex,$age_group,$value";
					}
				}
			}
			# p@values;
		}
	}
	close $in;
}