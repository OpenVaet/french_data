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

my $csv_parser            = Text::CSV_XS->new ({ binary => 1 });
my %deaths                = ();
my %deaths_by_departments = ();

load_deaths_data();
my ($deaths,
	$not_dead_in_france)  = (0, 0);
calculate_stats();
print_stats_by_departments();

p%deaths_by_departments;
say "$not_dead_in_france / $deaths";

sub load_deaths_data {
	my $file = 'data/insee_deathes_data.json';
	my $json;
	open my $in, '<:', $file;
	while (<$in>) {
		$json .= $_;
	}
	close $in;
	$json = decode_json($json);
	%deaths = %$json;
}

sub calculate_stats {
	for my $file (sort keys %deaths) {
		say $file;
		my ($cpt, $current)  = (0, 0);
		for my $line_num (sort{$a <=> $b} keys %{$deaths{$file}}) {
			$current++;
			$cpt++;
			if ($cpt == 100000) {
				$cpt = 0;
				say "Parsing [$current]";
			}
			my $birth_date    = $deaths{$file}->{$line_num}->{'birth_date'}   // die;
			my $death_date    = $deaths{$file}->{$line_num}->{'death_date'}   // die;
			my ($death_year)  = $death_date =~ /^(....)-..-..$/;
			die "death_date: $death_date" unless $death_year;
			my $death_place   = $deaths{$file}->{$line_num}->{'death_place'}  // die;
			my $age_in_days   = $deaths{$file}->{$line_num}->{'age_in_days'}  // die;
			my $age_in_years  = $deaths{$file}->{$line_num}->{'age_in_years'} // die;
			$deaths++;
			if ($death_place !~ /^.....$/) {
				say "abnormal format : [$death_place]";
				$not_dead_in_france++;
				next;
			}
			my ($department)  = $death_place =~ /(..).../;
			$deaths_by_departments{$department}->{$death_year}->{$age_in_years}++;	
			# p$deaths{$file}->{$line_num};
			# die;
			# if ($age_in_days < 0) {
			# 	p$deaths{$file}->{$line_num};
			# 	die "Negative age";
			# }
		}
	}
}

sub print_stats_by_departments {
	make_path('data') unless (-d 'data');
	open my $out, '>:utf8', 'data/deaths_by_age_groups_and_departments.csv';
	say $out "department,death_year,age_in_years,total_deaths";
	for my $department (sort keys %deaths_by_departments) {
		for my $death_year (sort keys %{$deaths_by_departments{$department}}) {
			for my $age_in_years (sort{$a <=> $b} keys %{$deaths_by_departments{$department}->{$death_year}}) {
				my $total_deaths = $deaths_by_departments{$department}->{$death_year}->{$age_in_years} // die;
				say $out "$department,$death_year,$age_in_years,$total_deaths";
			}
		}
	}
	close $out;
}