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
use Math::CDF qw(ppois);
use Statistics::Descriptive;
use Statistics::LineFit;
use DateTime;
use DateTime::Format::Strptime;
use File::Path qw(make_path);
use FindBin;
use lib "$FindBin::Bin/../../lib";

my %departments = ();
my %deaths      = ();
load_departments_data();
my %population  = ();
load_population();
my %death_rates = ();

my $deaths_by_departments_and_ages = 'data/deaths_by_age_groups_and_departments.csv';
open my $in, '<:utf8', $deaths_by_departments_and_ages;
while (<$in>) {
	chomp $_;
	my ($department, $death_year, $age_in_years, $total_deaths) = split ',', $_;
	next if $department eq 'department';
	next unless exists $departments{$department};
	# say "$department, $death_year, $age_in_years, $total_deaths";
	my $age_group = age_group_from_age($age_in_years);
	$deaths{$age_group}->{$death_year} += $total_deaths;
}
close $in;

for my $age_group (sort keys %deaths) {
	for my $year (sort{$a <=> $b} keys %{$deaths{$age_group}}) {
		my $deaths = $deaths{$age_group}->{$year} // die;
		my $population = $population{$age_group}->{$year};
		if (!$population && $age_group ne '0-1' && $year ne 2024) {
			die "age_group : $age_group, year : $year";
		}
		next unless $deaths && $population;
		my $per_1000 = $deaths * 1000 / $population;
		$death_rates{$age_group}->{$year} = $per_1000;
		# say "$age_group - $year - $deaths/$population";
	}
}

sub load_departments_data {
	my $file = 'departments.json';
	my $json;
	open my $in, '<:', $file;
	while (<$in>) {
		$json .= $_;
	}
	close $in;
	$json = decode_json($json);
	%departments = %$json;
}

sub load_population {
	my $population_file = 'data/population_by_age_and_department.csv';
	open my $in, '<:utf8', $population_file;
	while (<$in>) {
		chomp $_;
		my ($year, $department, $sex, $age_group, $value) = split ',', $_;
		next if $department eq 'department';
		next unless exists $departments{$department};
		$population{$age_group}->{$year} += $value;
	}
	close $in;
}

sub age_group_from_age {
	my ($age_in_years) = @_;
	my $insee_age_group;
	if ($age_in_years >= 0 && $age_in_years <= 0.99) {
		$insee_age_group = '0-4';
	} elsif ($age_in_years >= 1 && $age_in_years <= 4.99) {
		$insee_age_group = '0-4';
	} elsif ($age_in_years >= 5 && $age_in_years <= 9.99) {
		$insee_age_group = '5-9';
	} elsif ($age_in_years >= 10 && $age_in_years <= 14.99) {
		$insee_age_group = '10-14';
	} elsif ($age_in_years >= 15 && $age_in_years <= 19.99) {
		$insee_age_group = '15-19';
	} elsif ($age_in_years >= 20 && $age_in_years <= 24.99) {
		$insee_age_group = '20-24';
	} elsif ($age_in_years >= 25 && $age_in_years <= 29.99) {
		$insee_age_group = '25-29';
	} elsif ($age_in_years >= 30 && $age_in_years <= 34.99) {
		$insee_age_group = '30-34';
	} elsif ($age_in_years >= 35 && $age_in_years <= 39.99) {
		$insee_age_group = '35-39';
	} elsif ($age_in_years >= 40 && $age_in_years <= 44.99) {
		$insee_age_group = '40-44';
	} elsif ($age_in_years >= 45 && $age_in_years <= 49.99) {
		$insee_age_group = '45-49';
	} elsif ($age_in_years >= 50 && $age_in_years <= 54.99) {
		$insee_age_group = '50-54';
	} elsif ($age_in_years >= 55 && $age_in_years <= 59.99) {
		$insee_age_group = '55-59';
	} elsif ($age_in_years >= 60 && $age_in_years <= 64.99) {
		$insee_age_group = '60-64';
	} elsif ($age_in_years >= 65 && $age_in_years <= 69.99) {
		$insee_age_group = '65-69';
	} elsif ($age_in_years >= 70 && $age_in_years <= 74.99) {
		$insee_age_group = '70-74';
	} elsif ($age_in_years >= 75 && $age_in_years <= 79.99) {
		$insee_age_group = '75-79';
	} elsif ($age_in_years >= 80 && $age_in_years <= 84.99) {
		$insee_age_group = '80-84';
	} elsif ($age_in_years >= 85 && $age_in_years <= 89.99) {
		$insee_age_group = '85-89';
	} elsif ($age_in_years >= 90 && $age_in_years <= 94.99) {
		$insee_age_group = '90-94';
	} elsif ($age_in_years >= 95 && $age_in_years <= 125) {
		$insee_age_group = '95+';
	} else {
		die "age_in_years : $age_in_years";
	}
	return $insee_age_group;
}

for my $age_group (sort keys %death_rates) {
	my $year_2010 = $death_rates{$age_group}->{'2010'} // die;
	my $year_2011 = $death_rates{$age_group}->{'2011'} // die;
	my $year_2012 = $death_rates{$age_group}->{'2012'} // die;
	my $year_2013 = $death_rates{$age_group}->{'2013'} // die;
	my $year_2014 = $death_rates{$age_group}->{'2014'} // die;
	my $year_2015 = $death_rates{$age_group}->{'2015'} // die;
	my $year_2016 = $death_rates{$age_group}->{'2016'} // die;
	my $year_2017 = $death_rates{$age_group}->{'2017'} // die;
	my $year_2018 = $death_rates{$age_group}->{'2018'} // die;
	my $year_2019 = $death_rates{$age_group}->{'2019'} // die;
	my $year_2020 = $death_rates{$age_group}->{'2020'} // die;
	my $year_2021 = $death_rates{$age_group}->{'2021'} // die;
	my $year_2022 = $death_rates{$age_group}->{'2022'} // die;
	my $year_2023 = $death_rates{$age_group}->{'2023'} // die;
	say "\n$age_group";
	my $data_frame = "
  
# Defines the data as a named vector
data <- c(
`2010` = $year_2010, `2011` = $year_2011, `2012` = $year_2012, `2013` = $year_2013, `2014` = $year_2014,
`2015` = $year_2015, `2016` = $year_2016, `2017` = $year_2017, `2018` = $year_2018, `2019` = $year_2019,
`2020` = $year_2020, `2021` = $year_2021, `2022` = $year_2022, `2023` = $year_2023
)";
  	say $data_frame;
}